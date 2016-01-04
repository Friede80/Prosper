{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Control.Lens
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
prosperAddress :: Text
prosperAddress = "https://api.prosper.com/v1/"

prosperURL :: Text -> String
prosperURL target = (prosperAddress <> target) ^. unpacked

-- | Retrives the OAuth2 token from the Prosper server.
oauthToken :: String -> String -> String -> String -> IO ByteString
oauthToken clientID clientSecret userID password = do
    let body =
          [ "grant_type" := ("password" :: String)
          , "client_id" := clientID 
          , "client_secret" := clientSecret 
          , "username" := userID
          , "password" := password]
    resp <- post (prosperURL "security/oauth/token") body
    return $ utf8 # (resp ^. responseBody . key "access_token" . _String)

-- | Performs a GET request to the provided target using the provided OAuth2 token.
getTarget :: Options -> String -> ByteString -> IO (Response Value)
getTarget opts url token = getWith (opts & auth ?~ oauth2Bearer token) url >>= asJSON       
     
getPaginated :: Text -> (V.Vector Value -> [a]) -> ByteString -> IO [a]
getPaginated target parseFunc token = getPaginated' target parseFunc token 0
        
getPaginated' :: Text -> (V.Vector Value -> [a]) -> ByteString -> Int -> IO [a]
getPaginated' target parseFunc token offset = do
    resp <- getTarget opts (prosperURL target) token
    let result = resp ^. responseBody . key "result" . _Array 
    let vals = parseFunc result
    remainder <- if length result /= 25 
                    then return [] 
                    else getPaginated' target parseFunc token (offset + 25)
    return (vals ++ remainder)
    where
        opts = defaults & param "offset" .~ [offset ^. re _Show . packed]
        
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

getNoteRatings :: ByteString -> IO [Text]
getNoteRatings = getPaginated "notes/" parseRespForNotesRating

getPendingNoteRatings :: ByteString -> IO [Text]
getPendingNoteRatings token = do
    pendingIDs <- getPaginated "orders/" parseRespForPendingIDs token
    getRatingsOf token pendingIDs
                         
getRatingsOf :: ByteString -> [Integer] -> IO [Text]
getRatingsOf token listingIDs = 
    fmap (^.. _ratings) $ getTarget defaults (prosperURL searchQuery) token
    where
        searchQuery = ("search/listings/?listing_number=" ++ toString listingIDs) ^. packed
        toString xs = intercalate  "," $ map show xs
        _ratings = responseBody . key "result" . _Array . traverse . key "prosper_rating" . _String

getAvailableFunds :: ByteString -> IO (Maybe Double)
getAvailableFunds token = do    
    resp <- getTarget defaults (prosperURL "accounts/prosper/") token
    return $ resp ^? responseBody . key "available_cash_balance" . _Double

buyNote :: ByteString -> Text -> IO (Maybe Integer)
buyNote token rating = do 
    findRes <- findNote token rating
    case findRes of 
        (Just noteID) -> do 
            putStrLn $ "Buying: " ++ (show noteID)
            let opts = 
                  defaults 
                    & header "Accept" .~ ["application/json"] 
                    & header "Content-Type" .~ ["application/json"] 
                    & auth ?~ oauth2Bearer token
            let body = 
                  encode 
                    (BidRequests {bid_requests = [Bid {listing_id = noteID, bid_amount = 25.00}]})
            resp <- postWith opts (prosperURL "orders/") body
            return (Just noteID)
        Nothing -> do
            putStrLn $ "No notes found that match rating: " ++ (rating ^. unpacked) 
            return Nothing
        
findNote :: ByteString -> Text -> IO (Maybe Integer)
findNote token rating = do
    resp <- getTarget defaults (prosperURL searchQuery) token
    let proposedNote = V.head $ resp ^. responseBody . key "result" ._Array    
    return $ listingID proposedNote
    where
        searchQuery = ("search/listings/?" ++
                       "sort_by=effective_yield desc&" ++
                       "listing_term=36&" ++
                       "listing_title=\"Debt Consolidation\"&" ++
                       "exclude_listings_invested=true&" ++
                       "prosper_rating=\"" ++ (rating ^. unpacked) ++ "\"") ^. packed
        listingID x = x ^? key "listing_number" . _Integer
    
main :: IO ()
main = do
    (clientID:clientSecret:userID:password:xs) <- getArgs 
    token <- oauthToken clientID clientSecret userID password
    availableFunds <- getAvailableFunds token
    case availableFunds of
        (Just funds) ->
            if funds > 25
                then do
                    let numNotes = floor (funds / 25 )
                        myDist = [0, 0, 0.20, 0.20, 0.30, 0.25, 0.05]
                    notes <- getNoteRatings token
                    pendingNotes <- getPendingNoteRatings token
                    purchasedNotes <- mapM (buyNote token) $ recommendNotes (notes++pendingNotes) myDist numNotes
                    print purchasedNotes                
                else putStrLn "Insufficient funds"
        Nothing -> putStrLn "Unable to obtain available funds"
        
        