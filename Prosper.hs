{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}

import Control.Lens hiding (use)
import Data.Aeson
import Data.Aeson.Lens
import Data.Monoid ((<>))
import Data.Text.Strict.Lens
import Network.Wreq
import Data.Text (Text)
import Data.List
import Data.Maybe
import System.Environment
import System.IO (hFlush, stdout)
import GHC.Generics
import Data.ByteString.Internal
import Control.Concurrent.Async (mapConcurrently)
import qualified Data.Vector as V

import Notes
    
data URLPair where
    (:=>) :: String -> String -> URLPair
instance Show URLPair where
    show (key :=> val) = key ++ "=" ++ val
    showList xs = (++) $ intercalate "&" . map show $ xs

data URL = URL
    { addr :: String
    , base :: String
    , query :: [URLPair] }
use :: URL -> String
use url = (addr url) ++ (base url) ++ "?" ++ (show . query $ url)
    
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
prosperUrl base = prosperUrlWith base []

prosperUrlWith :: String -> [URLPair] -> String
prosperUrlWith base' query' = use $ URL {addr = prosperAddress, base = base', query = query'}

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
getTarget :: Options -> String -> ByteString -> IO (Response Value)
getTarget opts url token = getWith (opts & auth ?~ oauth2Bearer token) url >>= asJSON       
     
getPaginated :: String -> (V.Vector Value -> [a]) -> ByteString -> IO [a]
getPaginated url parseFunc token = do
    resp <- getTarget defaults url token
    let totalResults = resp ^? responseBody . key "total_count" . _Double
        vals = extractVals resp
    case totalResults of
        (Just results) -> do
            let n = ceiling $ results / 25
                offsets = take (n-1) [25,50..]
            restOfVals <- fmap concat $ mapConcurrently someFunc offsets
            return (vals ++ restOfVals) 
        Nothing -> return vals
    where
        extractVals response = parseFunc $ response ^. responseBody . key "result" . _Array
        someFunc offset' = do
            let opts = defaults & param "offset" .~ [offset' ^. re _Show . packed]
            resp' <- getTarget opts url token
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
                                      
getNoteRatings :: ByteString -> IO [Text]
getNoteRatings = getPaginated (prosperUrl "notes/") parseRespForNotesRating

getPendingNoteRatings :: ByteString -> IO [Text]
getPendingNoteRatings token = do
    pendingIDs <- getPaginated (prosperUrl "orders/") parseRespForPendingIDs token
    getRatingsOf token pendingIDs
                         
getRatingsOf :: ByteString -> [Integer] -> IO [Text]
getRatingsOf token listingIDs = 
    fmap (^.. _ratings) $ getTarget defaults (prosperUrlWith "search/listings/" query) token
    where
        query = [ "listing_number" :=> (toString listingIDs)]
        toString xs = intercalate  "," $ map show xs
        _ratings = responseBody . key "result" . _Array . traverse . key "prosper_rating" . _String

getAvailableFunds :: ByteString -> IO (Maybe Double)
getAvailableFunds token = do    
    resp <- getTarget defaults (prosperUrl "accounts/prosper/") token
    return $ resp ^? responseBody . key "available_cash_balance" . _Double

buyNotes :: ByteString -> [Text] -> IO ([Integer])
buyNotes token ratings = do 
    notesToPurchase <- fmap concat . mapM selectNotes . notes $ ratings 
    let opts = 
              defaults 
                & header "Accept" .~ ["application/json"] 
                & header "Content-Type" .~ ["application/json"] 
                & auth ?~ oauth2Bearer token
        body = encode (BidRequests {bid_requests = (bids notesToPurchase)})                
    resp <- postWith opts (prosperUrl "orders/") body
    -- TODO: Verify notes were purchased successfully
    return notesToPurchase
    where        
        notes = map (\l@(x:xs) -> (x, length l)) . group . sort    
        bids = map (\x -> (Bid {listing_id = x, bid_amount = 25.00}))        
        selectNotes (rating, count) = do
            let query rating = [ "sort_by" :=> "effective_yield desc"
                               , "listing_term" :=> "36"
                               , "listing_title" :=> "Debt Consolidation"
                               , "exclude_listings_invested" :=> "true"
                               , "prosper_rating" :=> (rating ^. unpacked) ] 
            findRes <- findNotes token (query rating)
            return $ take count findRes 
                
                       
findNotes :: ByteString -> [URLPair] -> IO ([Integer])
findNotes token query = getPaginated (prosperUrlWith "search/listings/" query) parseListings token
    
main :: IO ()
main = do
    (clientID:clientSecret:userID:password:xs) <- getArgs 
    token <- oauthToken clientID clientSecret userID password
    availableFunds <- getAvailableFunds token
    case availableFunds of
        (Just funds) ->
            if funds >= 25
                then do                    
                    currentNotes <- getNoteRatings token
                    pendingNotes <- getPendingNoteRatings token
                    let numNotes = floor (funds / 25 )
                        myDist = [0, 0, 0.20, 0.20, 0.30, 0.25, 0.05]
                        notes = currentNotes ++ pendingNotes
                    purchasedNotes <- buyNotes token $ recommendNotes notes myDist numNotes
                    print purchasedNotes                
                else putStrLn "Insufficient funds"
        Nothing -> putStrLn "Unable to obtain available funds"
        