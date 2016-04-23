{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Concurrent.Async (mapConcurrently)
import           Control.Lens             hiding (use)
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.ByteString.Internal
import           Data.List
import           Data.Maybe
import           Data.Text                (Text)
import           Data.Text.Strict.Lens
import           Data.Time.Clock
import           Data.Time.Format
import qualified Data.Vector              as V
import           GHC.Generics
import           Network.Wreq
import           System.Directory
import           System.Environment
import           System.IO                hiding (utf8)

import           Notes

-----------------------------------------------------------------------------------
-- Data Definitions
-----------------------------------------------------------------------------------
data BidRequests  = BidRequests {
    bid_requests :: [Bid]
    } deriving (Generic, Show)
instance ToJSON BidRequests

data Bid = Bid {
      bid_amount :: Double
    , listing_id :: Integer
    } deriving (Generic, Show)
instance ToJSON Bid

-----------------------------------------------------------------------------------
-- String Utilites
-----------------------------------------------------------------------------------
prosperAddress :: String
prosperAddress = "https://api.prosper.com/v1/"

prosperUrl :: String -> String
prosperUrl = (++) prosperAddress

-----------------------------------------------------------------------------------
-- Core Web Utilites
-----------------------------------------------------------------------------------

-- | Performs a GET request and packs the results as JSON.
getTarget :: String -> Options -> IO (Response Value)
getTarget url aOpts = getWith aOpts url >>= asJSON

-- | Performs GET requests and handles paginated responsonses.
getPaginated :: String -> Options -> (V.Vector Value -> [a]) -> IO [a]
getPaginated url aOpts parseFunc = do
    resp <- getTarget url aOpts
    let totalResults = resp ^? responseBody . key "total_count" . _Double
        vals = extractVals resp
    case totalResults of
        (Just results) -> do
            let n = ceiling $ results / 25
                offsets = take (n-1) [25,50..]
            restOfVals <- concat <$> mapConcurrently getPage offsets
            return (vals ++ restOfVals)
        Nothing -> return vals
    where
        extractVals response = parseFunc $ response ^. responseBody . key "result" . _Array
        getPage offset = do
            let opts = aOpts & param "offset" .~ [offset ^. re _Show . packed]
            resp' <- getTarget url opts
            return $ extractVals resp'

-----------------------------------------------------------------------------------
-- Prosper GET Tools
-----------------------------------------------------------------------------------

-- | Gets a list of all the ratings of currently owned Notes
getNoteRatings :: Options -> IO [Text]
getNoteRatings aOpts = getPaginated (prosperUrl "notes/") aOpts parseRespForNotesRating
    where
        parseRespForNotesRating resp = resp ^.. traverse . key "prosper_rating" . _String

-- | Gets a list of all the ratings of pending Notes
getPendingNoteRatings :: Options -> IO [Text]
getPendingNoteRatings aOpts = do
    pendingIDs <- getPaginated (prosperUrl "orders/") aOpts parseRespForPendingIDs
    getRatingsOf aOpts pendingIDs
    where
        parseRespForPendingIDs result =
            let bids = V.concat $ result ^.. traverse . key "bid_requests" . _Array
                pendingBids = V.toList $ V.filter isPending bids
                isPending x = (x ^. key "bid_status" . _String) == "PENDING"
                listingID x = x ^? key "listing_id" . _Integer
            in mapMaybe listingID pendingBids

-- | Looks up the ratings for a given set of note IDs
getRatingsOf :: Options -> [Integer] -> IO [Text]
getRatingsOf _ [] = return []
getRatingsOf aOpts listingIDs =
    (^.. _ratings) <$> getTarget (prosperUrl "search/listings/") opts
    where
        opts = aOpts & param "listing_number" .~ [toText listingIDs]
        toText xs = intercalate  ","  (map show xs) ^. packed
        _ratings = responseBody . key "result" . _Array . traverse . key "prosper_rating" . _String

-- | Gets the currently available funds from one's Proser account
getAvailableFunds :: Options -> IO (Maybe Double)
getAvailableFunds aOpts = do
    resp <- getTarget (prosperUrl "accounts/prosper/") aOpts
    return $ resp ^? responseBody . key "available_cash_balance" . _Double

-- | Queries Prosper for all notes matching the given options and returns their note IDs
findNotes :: Options -> IO [Integer]
findNotes aOpts = getPaginated (prosperUrl "search/listings/") aOpts parseListings
    where
        parseListings resp = resp ^.. traverse . key "listing_number" . _Integer

-----------------------------------------------------------------------------------
-- Prosper POST Tools
-----------------------------------------------------------------------------------

-- | Attempts to purchase notes matching the provided Ratings
buyNotes :: Options -> [Text] -> IO [Integer]
buyNotes aOpts ratings = do
    notesToPurchase <- fmap concat . mapM selectNotes . notes $ ratings
    if null notesToPurchase
        then return []
        else do
            let opts = aOpts
                        & header "Accept" .~ ["application/json"]
                        & header "Content-Type" .~ ["application/json"]
                body = encode BidRequests {bid_requests = bids notesToPurchase}
            _ <- postWith opts (prosperUrl "orders/") body
            -- TODO: Verify notes were purchased successfully
            return notesToPurchase
    where
        notes = map (\l@(x:_) -> (x, length l)) . group . sort
        bids = map (\x -> Bid {listing_id = x, bid_amount = 25.00})
        selectNotes (rating, count) = do
            let opts = aOpts
                        & param "sort_by" .~ ["effective_yield desc"]
                        & param "listing_term" .~ ["36"]
                        & param "listing_title" .~ ["Debt Consolidation"]
                        & param "exclude_listings_invested" .~ ["true"]
                        & param "prosper_rating" .~ [rating]
            findRes <- findNotes opts
            return $ take count findRes

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


-----------------------------------------------------------------------------------
-- Main Application
-----------------------------------------------------------------------------------
main :: IO ()
main = do
    --Initialization
    (configFilePath, nFlag) <- getArgs >>= validateArgs
    (clientID,clientSecret,userID,password,distribution) <- readConfig <$> readFile configFilePath
    logFile <- openFile "Prosper.log" AppendMode
    hPutStrLn logFile . formatTime defaultTimeLocale "%c" =<< getCurrentTime

    --Get Oauth2 token and use it to create autorized HTTP options
    token <- oauthToken clientID clientSecret userID password
    let authOpts = defaults & auth ?~ oauth2Bearer token

    --Try to purchase notes
    availableFunds <- getAvailableFunds authOpts
    case availableFunds of
        (Just funds) ->
            if funds >= 25
                then do
                    currentNotes <- getNoteRatings authOpts
                    pendingNotes <- getPendingNoteRatings authOpts
                    let numNotes = floor (funds / 25 )
                        notes = currentNotes ++ pendingNotes
                        rn = recommendNotes notes distribution numNotes
                    if nFlag
                      then print rn
                      else hPrint logFile =<< buyNotes authOpts rn
                else hPutStrLn logFile "Insufficient funds"
        Nothing -> hPutStrLn logFile "Unable to obtain available funds"
    hClose logFile
-----------------------------------------------------------------------------------
-- Random
-----------------------------------------------------------------------------------
validateArgs :: [String] -> IO (FilePath, Bool)
validateArgs args = case args of
  [filePath] -> verified filePath >>= (\x -> return (x,False))
  ["-n", filePath] -> verified filePath >>= (\x -> return (x,True))
  [filePath, "-n"] -> verified filePath >>= (\x -> return (x,True))
  _ -> error usage
  where
    verified fp = do
      exists <- doesFileExist fp
      if exists then return fp else error "Error: Config file not found"
    usage = "Usage: Prosper [options] config_filepath\n" ++
            "  options:\n" ++
            "    -n, Do nothing, but show what notes would be bought"

readConfig :: String -> (String,String,String,String,[Double])
readConfig config = ( config ^. key "clientID" . _String . unpacked
                    , config ^. key "clientSecret" . _String . unpacked
                    , config ^. key "userID" . _String . unpacked
                    , config ^. key "password" . _String . unpacked
                    , config ^.. key "distribution" . _Array . traverse . _Double )
