{- A server made to recieve a message outbound from Slack (inbound to the bot.)-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module OutgoingWebhook where

import           Blaze.ByteString.Builder (copyByteString)
import qualified Data.ByteString.Char8    as B
import           Data.IORef               (IORef)
import qualified Data.Map.Strict          as M
import           Data.Maybe               (fromMaybe)
import           GHC.Generics             (Generic)
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

listen :: IORef (M.Map String String) -> IO()
listen state = withStdoutLogger $ \aplogger -> do
    let port = 3000
    putStrLn $ "Listening on port " ++ show port
    run port $ app state aplogger

app :: IORef (M.Map String String) -> ApacheLogger -> Application -- Request -> (Response -> a) -> a
app state aplogger req response = do
    aplogger req status (Just len)
    params <- parseRawQS <$> requestBody req
    om <- parseQS params
    response $ index state afkHandler om
  where
    status = status200
    len = 0 -- TODO: get length of response

    parseRawQS :: B.ByteString -> [(B.ByteString, B.ByteString)]
    parseRawQS qs = (\[x,y] -> (x, y)) <$> B.split '=' <$> B.split '&' qs

    parseQS :: [(B.ByteString, B.ByteString)] -> IO OutgoingMessage
    parseQS params =
        return OutgoingMessage {
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
        lookup' k = B.unpack $ fromMaybe ("Key not found" :: B.ByteString) (lookup k params)

index :: IORef (M.Map String String) -> Handler -> OutgoingMessage -> Response
index s h om = responseBuilder status200 [("Content-Type", "text/html")] (mconcat (map copyByteString [ "Thanks.\n" ]))

  -- response <- responseBuilder status200 [("Content-Type", "text/html")] (mconcat (map copyByteString [ "Thanks.\n" ]))
  -- return response

afkHandler :: Handler
afkHandler = undefined
