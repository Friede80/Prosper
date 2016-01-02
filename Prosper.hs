{-# LANGUAGE OverloadedStrings #-}

import Control.Lens
import Data.Aeson
import Data.Aeson.Lens
import Data.Monoid ((<>))
import Data.Text.Strict.Lens
import Network.Wreq
import Data.Text (Text)
import Data.List
import System.Environment

import qualified Data.Vector as V
import Debug.Trace

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

-- | Performs a GET request to the provided target using the provided
-- OAuth token.
getTarget :: Options -> String -> Text -> IO (Response Value)
getTarget opts url token = do
    getWith (opts & auth ?~ oauth2Bearer token') url >>= asJSON 
    where
        token' = utf8 # token

-- | Retrives the list of currently owned notes from Prosper.
--
-- Note: The query responses are paginated to 25 results per response.
getNotesList :: Text -> IO [Text]
getNotesList token = getNotesList' token 0

getNotesList' :: Text -> Int -> IO [Text]
getNotesList' token offset = do
    resp <- getTarget opts (prosperURL "notes/") token
    let ratings = resp ^.. _ratings
    remainder <- if length ratings /= 25 then return [] else getNotesList' token (offset + 25)
    return (ratings ++ remainder)
    where
        opts = defaults & param "offset" .~ [offset ^. re _Show . packed]
        _ratings = responseBody . key "result" . _Array . traverse . key "prosper_rating" . _String

getPendingIDs :: Text -> Int -> IO [Maybe Integer]
getPendingIDs token offset = do
    resp <- getTarget opts (prosperURL "orders/") token
    let orders = resp ^. _orders
    let bids = V.concat $ orders ^.. traverse . key "bid_requests" . _Array
    let pendingBids = V.toList $ V.filter isPending bids
    let pendingIDs = map getIDs pendingBids
    remainder <- if length orders /= 25 then return [] else getPendingIDs token (offset + 25)
    return (pendingIDs ++ remainder)
    where
        opts = defaults & param "offset" .~ [offset ^. re _Show . packed]
        _orders = responseBody . key "result" . _Array 
        isPending x = (x ^. key "bid_status" . _String) == "PENDING"
        getIDs x = x ^? key "listing_id" . _Integer

getRatingOf :: Text -> [Integer] -> IO [Text]
getRatingOf token listingIDs = do   
    resp <- getTarget defaults (prosperURL searchQuery) token
    return $ resp ^.. _ratings
    where
        searchQuery = ("search/listings/?listing_number=" ++ toString listingIDs) ^. packed
        toString xs = intercalate  "," $ map show xs
        _ratings = responseBody . key "result" . _Array . traverse . key "prosper_rating" . _String

reduceMaybe :: [Maybe a] -> [a]
reduceMaybe = foldl (\acc x -> extractMaybe x ++ acc) []
    where
        extractMaybe Nothing = []
        extractMaybe (Just n) = [n]
        
main :: IO ()
main = do
    (clientID:clientSecret:userID:password:xs) <- getArgs 
    token <- oauthToken clientID clientSecret userID password
    pendingIDs <- getPendingIDs token 0
    pendingNotes <- getRatingOf token $ reduceMaybe pendingIDs 
    notes <- getNotesList token
    print $ recommendNotes (notes++pendingNotes) [0, 0, 0.20, 0.20, 0.30, 0.25, 0.05] 3
    