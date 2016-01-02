{-# LANGUAGE OverloadedStrings #-}

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
import qualified Data.Vector as V

import Notes

-- String Constants
prosperAddress :: Text
prosperAddress = "https://api.prosper.com/v1/"

prosperURL :: Text -> String
prosperURL target = (prosperAddress <> target) ^. unpacked

-- | Retrives the OAuth2 token from the Prosper server.
oauthToken :: String -> String -> String -> String -> IO Text
oauthToken clientID clientSecret userID password = do
    let opts =
          defaults
            & header "Accept" .~ ["application/json"]
            & header "Content-Type" .~ ["application/x-www-form-urlencoded"]
    let body =
          [ "grant_type" := ("password" :: String)
          , "client_id" := clientID 
          , "client_secret" := clientSecret 
          , "username" := userID
          , "password" := password]
    resp <- postWith opts (prosperURL "security/oauth/token") body
    return (resp ^. responseBody . key "access_token" . _String)

-- | Performs a GET request to the provided target using the provided OAuth2 token.
getTarget :: Options -> String -> Text -> IO (Response Value)
getTarget opts url token = do
    getWith (opts & auth ?~ oauth2Bearer token') url >>= asJSON 
    where
        token' = utf8 # token        
     
getPaginated :: Text -> (V.Vector Value -> [a]) -> Text -> IO [a]
getPaginated target parseFunc token = getPaginated' target parseFunc token 0
        
getPaginated' :: Text -> (V.Vector Value -> [a]) -> Text -> Int -> IO [a]
getPaginated' target parseFunc token offset = do
    resp <- getTarget opts (prosperURL target) token
    let result = resp ^. responseBody . key "result" . _Array 
    let vals = parseFunc result
    remainder <- if length result /= 25 then return [] else getPaginated' target parseFunc token (offset + 25)
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

getNoteRatings :: Text -> IO [Text]
getNoteRatings = getPaginated "notes/" parseRespForNotesRating

getPendingNoteRatings :: Text -> IO [Text]
getPendingNoteRatings token = do
    pendingIDs <- getPaginated "orders/" parseRespForPendingIDs token
    getRatingsOf token pendingIDs
                         
getRatingsOf :: Text -> [Integer] -> IO [Text]
getRatingsOf token listingIDs = do   
    resp <- getTarget defaults (prosperURL searchQuery) token
    return $ resp ^.. _ratings
    where
        searchQuery = ("search/listings/?listing_number=" ++ toString listingIDs) ^. packed
        toString xs = intercalate  "," $ map show xs
        _ratings = responseBody . key "result" . _Array . traverse . key "prosper_rating" . _String
        
main :: IO ()
main = do
    (clientID:clientSecret:userID:password:xs) <- getArgs 
    token <- oauthToken clientID clientSecret userID password
    notes <- getNoteRatings token
    pendingNotes <- getPendingNoteRatings token
    print $ recommendNotes (notes++pendingNotes) [0, 0, 0.20, 0.20, 0.30, 0.25, 0.05] 3
        