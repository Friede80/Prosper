{-# LANGUAGE OverloadedStrings #-}

import Control.Lens
import Data.Aeson
import Data.Aeson.Lens
import Data.Monoid ((<>))
import Data.Text.Strict.Lens
import Network.Wreq
import Data.Text (Text)
import System.Environment

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
          [ "grant_type" := ("password" :: Text)
          , "client_id" := (clientID ^. packed)
          , "client_secret" := (clientSecret ^. packed)
          , "username" := (userID ^. packed)
          , "password" := (password ^. packed)]
    resp <- postWith opts (prosperURL "security/oauth/token") body
    return (resp ^. responseBody . key "access_token" . _String)

-- | Performs a GET request to the provided target using the provided
-- OAuth token.
getTarget :: Options -> String -> Text -> IO (Response Value)
getTarget opts url token = do
    getWith (opts & header "Authorization" .~ rawr) url >>= asJSON
    where
        rawr = ["bearer " <> utf8 # token]

getNotesList :: Text -> IO [Text]
getNotesList token = _getNotesList token 0
 
-- | Retrives the list of currently owned notes from Prosper.
--
-- Note: The query responses are paginated to 25 results per response.
_getNotesList :: Text -> Int -> IO [Text]
_getNotesList token offset = do
    resp <- getTarget opts (prosperURL "notes/") token
    let ratings = resp ^.. _ratings
    remainder <- if length ratings == 25 then return [] else _getNotesList token (offset + 25)
    return (ratings ++ remainder)
    where
        opts = defaults & param "offset" .~ [offset ^. re _Show . packed]
        _ratings = responseBody . key "result" . _Array . traverse . key "prosper_rating" . _String

main :: IO ()
main = do
    (clientID:clientSecret:userID:password:xs) <- getArgs 
    token <- oauthToken clientID clientSecret userID password
    notes <- getNotesList token
    print $ recommendNotes notes [0.0,0.0,0.0,0.5,0.5,0.0,0.0] 5