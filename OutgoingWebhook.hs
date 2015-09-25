{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

{- A server made to recieve a message outbound from Slack (inbound to the bot.)-}

module OutgoingWebhook where

import           Blaze.ByteString.Builder (copyByteString)
import qualified Data.ByteString.Char8    as B
import           GHC.Generics             (Generic)
import qualified Data.Map as M
import           Data.Maybe               (fromMaybe)
import           Data.IORef
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



listen :: IORef (M.Map String String) -> IO ()
listen state = withStdoutLogger $ \aplogger -> do
    let port = 3000
    putStrLn $ "Listening on port " ++ show port
    run port $ app state aplogger

app :: IORef (M.Map String String) -> ApacheLogger -> Application
app state aplogger req response = do
    aplogger req status200 (Just 0)
    params <- parseRawQS <$> requestBody req
    let om = parseQS params
    mytuple <- afkHandler state om
    response $ index mytuple
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


index :: String -> Response
index x =
  responseBuilder
    status200
    [("Content-Type", "text/html")]
    (mconcat (map copyByteString [ "Thanks.\n", B.pack x]))

afkHandler :: IORef (M.Map String String) -> OutgoingMessage -> IO String
afkHandler state om = do
  print om
  modifyIORef state $ \x -> M.insert (userName om) (text om) x
  mymap <- readIORef state
  print mymap
  return "yes"
