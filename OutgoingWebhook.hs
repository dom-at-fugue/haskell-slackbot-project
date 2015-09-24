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

type UserRecord = (String, String)
type Handler = OutgoingMessage -> UserRecord

listen :: IORef (M.Map a b) -> IO ()
listen state = withStdoutLogger $ \aplogger -> do
    let port = 3000
    putStrLn $ "Listening on port " ++ show port
    run port $ app state aplogger

app :: IORef (M.Map a b) -> ApacheLogger -> Application
app state aplogger req response = do
    aplogger req status (Just len)
    params <- parseRawQS <$> requestBody req
    let om = parseQS params
    response $ index state
  where
    status = status200
    len = 0 -- TODO: get length of response

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


index :: IORef (M.Map a b) -> Response
index state =
  responseBuilder
    status200
    [("Content-Type", "text/html")]
    (mconcat (map copyByteString [ "Thanks.\n" ]))

afkHandler :: IORef (M.Map a b) -> Handler
afkHandler = undefined
