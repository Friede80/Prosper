{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}

import Control.Lens hiding (use)
import Data.Aeson
import Data.Aeson.Lens
import Data.Text.Strict.Lens
import Network.Wreq
import Data.Text (Text)
import Data.List
import Data.Maybe
import System.Environment
import GHC.Generics
import Data.ByteString.Internal
import Control.Concurrent.Async (mapConcurrently)
import qualified Data.Vector as V

import Notes
    
data BidRequests  = BidRequests {
    bid_requests :: [Bid]
    } deriving (Generic, Show)
instance ToJSON BidRequests

data Bid = Bid {
      bid_amount :: Double
    , listing_id :: Integer
    } deriving (Generic, Show)
instance ToJSON Bid

-- String Constants
prosperAddress :: String
prosperAddress = "https://api.prosper.com/v1/"

prosperUrl :: String -> String
prosperUrl = (++) prosperAddress

-- | Retrives the OAuth2 token from the Prosper server.
oauthToken :: String -> String -> String -> String -> IO ByteString
oauthToken clientID clientSecret userID password = do
    let body =
          [ "grant_type" := ("password" :: String)
          , "client_id" := clientID 
          , "client_secret" := clientSecret 
          , "username" := userID
          , "password" := password]
    resp <- post (prosperUrl "security/oauth/token") body
    return $ utf8 # (resp ^. responseBody . key "access_token" . _String)

-- | Performs a GET request to the provided target using the provided OAuth2 token.
getTarget :: String -> Options -> IO (Response Value)
getTarget url aOpts = getWith aOpts url >>= asJSON       
     
getPaginated :: String -> Options -> (V.Vector Value -> [a]) -> IO [a]
getPaginated url aOpts parseFunc = do
    resp <- getTarget url aOpts
    let totalResults = resp ^? responseBody . key "total_count" . _Double
        vals = extractVals resp
    case totalResults of
        (Just results) -> do
            let n = ceiling $ results / 25
                offsets = take (n-1) ([25,50..] :: [Int])
            restOfVals <- fmap concat $ mapConcurrently getPage offsets
            return (vals ++ restOfVals) 
        Nothing -> return vals
    where
        extractVals response = parseFunc $ response ^. responseBody . key "result" . _Array
        getPage offset = do
            let opts = aOpts & param "offset" .~ [offset ^. re _Show . packed]
            resp' <- getTarget url opts 
            return $ extractVals resp'
        
parseRespForPendingIDs :: V.Vector Value -> [Integer]
parseRespForPendingIDs result = catMaybes $ map listingID pendingBids
    where        
        bids = V.concat $ result ^.. traverse . key "bid_requests" . _Array
        pendingBids = V.toList $ V.filter isPending bids
        isPending x = (x ^. key "bid_status" . _String) == "PENDING"
        listingID x = x ^? key "listing_id" . _Integer        
        
parseRespForNotesRating :: V.Vector Value -> [Text]
parseRespForNotesRating resp = resp ^.. traverse 
                                      . key "prosper_rating" 
                                      . _String

parseListings :: V.Vector Value -> [Integer]
parseListings resp = resp ^.. traverse
                            . key "listing_number"
                            . _Integer
                                      
getNoteRatings :: Options -> IO [Text]
getNoteRatings aOpts = getPaginated (prosperUrl "notes/") aOpts parseRespForNotesRating

getPendingNoteRatings :: Options -> IO [Text]
getPendingNoteRatings aOpts = do
    pendingIDs <- getPaginated (prosperUrl "orders/") aOpts parseRespForPendingIDs 
    getRatingsOf aOpts pendingIDs
                         
getRatingsOf :: Options -> [Integer] -> IO [Text]
getRatingsOf aOpts listingIDs = 
    fmap (^.. _ratings) $ getTarget (prosperUrl "search/listings/") opts
    where
        opts = aOpts & param "listing_number" .~ [(toText listingIDs)]
        toText xs = (intercalate  "," $ map show xs) ^. packed
        _ratings = responseBody . key "result" . _Array . traverse . key "prosper_rating" . _String

getAvailableFunds :: Options -> IO (Maybe Double)
getAvailableFunds aOpts = do    
    resp <- getTarget (prosperUrl "accounts/prosper/") aOpts
    return $ resp ^? responseBody . key "available_cash_balance" . _Double

buyNotes :: Options -> [Text] -> IO ([Integer])
buyNotes aOpts ratings = do 
    notesToPurchase <- fmap concat . mapM selectNotes . notes $ ratings 
    let opts = aOpts 
                & header "Accept" .~ ["application/json"] 
                & header "Content-Type" .~ ["application/json"] 
        body = encode (BidRequests {bid_requests = (bids notesToPurchase)})                
    _ <- postWith opts (prosperUrl "orders/") body
    -- TODO: Verify notes were purchased successfully
    return notesToPurchase
    where        
        notes = map (\l@(x:_) -> (x, length l)) . group . sort    
        bids = map (\x -> (Bid {listing_id = x, bid_amount = 25.00}))        
        selectNotes (rating, count) = do
            let opts = aOpts 
                        & param "sort_by" .~ ["effective_yield desc"]
                        & param "listing_term" .~ ["36"]
                        & param "listing_title" .~ ["Debt Consolidation"]
                        & param "exclude_listings_invested" .~ ["true"]
                        & param "prosper_rating" .~ [rating]  
            findRes <- findNotes opts
            return $ take count findRes 
                                       
findNotes :: Options -> IO ([Integer])
findNotes aOpts = getPaginated (prosperUrl "search/listings/") aOpts parseListings 
    
main :: IO ()
main = do
    (clientID:clientSecret:userID:password:_) <- getArgs 
    token <- oauthToken clientID clientSecret userID password
    let authOpts = defaults & auth ?~ oauth2Bearer token
    availableFunds <- getAvailableFunds authOpts
    case availableFunds of
        (Just funds) ->
            if funds >= 25
                then do                    
                    currentNotes <- getNoteRatings authOpts
                    pendingNotes <- getPendingNoteRatings authOpts
                    let numNotes = floor (funds / 25 ) :: Int
                        myDist = [0, 0, 0.20, 0.20, 0.30, 0.25, 0.05]
                        notes = currentNotes ++ pendingNotes
                    purchasedNotes <- buyNotes authOpts $ recommendNotes notes myDist numNotes
                    print purchasedNotes                
                else putStrLn "Insufficient funds"
        Nothing -> putStrLn "Unable to obtain available funds"
        