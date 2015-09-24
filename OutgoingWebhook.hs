{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

{- A server made to recieve a message outbound from Slack (inbound to the bot.)-}

module OutgoingWebhook where

import           Blaze.ByteString.Builder (copyByteString)
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

listen :: IO ()
listen = withStdoutLogger $ \aplogger -> do
    let port = 3000
    putStrLn $ "Listening on port " ++ show port
    run port $ app aplogger

app :: ApacheLogger -> Application
app aplogger req response = do
    aplogger req status (Just len)
    response index
  where
    status = status200
    len = 0 -- TODO: get length of response

index :: Response
index =
  responseBuilder
    status200
    [("Content-Type", "text/html")]
    (mconcat (map copyByteString [ "Thanks.\n" ]))

afkHandler :: Handler
afkHandler = undefined
