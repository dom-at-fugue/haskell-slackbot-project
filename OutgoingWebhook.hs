{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

{- A server made to recieve a message outbound from Slack (inbound to the bot.)-}

module OutgoingWebhook where

import           Blaze.ByteString.Builder (copyByteString)
import qualified Data.ByteString.Char8    as B
import           Data.IORef
import qualified Data.Map                 as M
import           Data.Maybe               (fromMaybe)
import qualified Data.Text                as T
import           GHC.Generics             (Generic)
import qualified IncomingWebhook          as I
import           Network.HTTP.Types       (status200)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Logger       (ApacheLogger, withStdoutLogger)

data OutgoingMessage = OutgoingMessage {
    token       :: String
  , teamId      :: String
  , teamDomain  :: String
  , channelId   :: String
  , channelName :: String
  , timestamp   :: String
  , userId      :: String
  , userName    :: String
  , text        :: String
  , triggerWord :: String }
  deriving (Show, Generic)

type Username = String
type AfkStatus = (Bool, String)
type AfkTable = IORef (M.Map Username AfkStatus)

listen :: AfkTable -> IO ()
listen state = withStdoutLogger $ \aplogger -> do
    let port = 3000
    putStrLn $ "Listening on port " ++ show port
    run port $ app state aplogger

app :: AfkTable -> ApacheLogger -> Application
app state aplogger req response = do
    params <- parseRawQS <$> requestBody req
    let om = parseQS params
    afkHandler state om
    aplogger req status200 (Just 0)
    response index
  where
    parseRawQS :: B.ByteString -> [(B.ByteString, B.ByteString)]
    parseRawQS qs = (\[x,y] -> (x, y)) <$> B.split '=' <$> B.split '&' qs

    parseQS :: [(B.ByteString, B.ByteString)] -> OutgoingMessage
    parseQS params = OutgoingMessage {
            token = lookup' "token"
          , teamId = lookup' "team_id"
          , teamDomain = lookup' "team_domain"
          , channelId   = lookup' "channel_id"
          , channelName = lookup' "channel_name"
          , timestamp    = lookup' "timestamp"
          , userId      = lookup' "user_id"
          , userName    = lookup' "user_name"
          , text         = lookup' "text"
          , triggerWord = lookup' "trigger_word"
        }
      where
        lookup' :: B.ByteString -> String
        lookup' k = B.unpack $
          fromMaybe ("Key not found" :: B.ByteString) (lookup k params)

-- Herein, we are being nice and appreciative to Slack.
index :: Response
index =
  responseBuilder
    status200
    [("Content-Type", "text/html")]
    (mconcat (map copyByteString [ "Thanks.\n"]))


afkHandler :: AfkTable -> OutgoingMessage -> IO ()
afkHandler state om = case () of
  () | strip (triggerWord om) == "afk" -> do
        modifyIORef state $ \x -> M.insert (userName om) (True, text om) x
        mymap <- readIORef state
        putStrLn "Was AFK."
        print mymap
     | strip (triggerWord om) == "back" -> do
        modifyIORef state $ \x -> M.adjust (const (False, text om)) (userName om) x
        mymap <- readIORef state
        putStrLn "Was Back."
        print mymap
     | strip (triggerWord om) == "status" -> do
        mymap <- readIORef state
        putStrLn "Was Status."
        print mymap
        I.send "#bottester" $ show mymap
     | otherwise -> print $ "Was otherwise." ++ triggerWord om
  where
    strip :: String -> String
    strip = T.unpack . T.strip . T.pack
--
-- afkHandler :: AfkTable -> OutgoingMessage -> IO ()
-- afkHandler state om = do
--   modifyIORef state $ \x -> M.insert (userName om) (True, text om) x
--   mymap <- readIORef state
--   print mymap
