{- Message incoming to Slack (outbound from bot). -}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module IncomingWebhook where

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Lens
import qualified Data.Text       as T
import           GHC.Generics    (Generic)
import           Network.Wreq
import System.Environment


slackURL :: IO String
slackURL = getEnv "AFKBOT_TOKEN"

data IncomingMessage = IncomingMessage {
  channel :: String
  , text  :: String }
  deriving (Show, Generic)

instance FromJSON IncomingMessage
instance ToJSON IncomingMessage

send :: String -> String -> IO()
send c t = do

    let request = IncomingMessage c t
    url <- slackURL
    r <- post url (encode request)

    let datas = r ^. responseBody . key "data" . _String
        ua    = r ^. responseBody . key "headers" . key "User-Agent" . _String

    (putStrLn . T.unpack) datas
    (putStrLn . T.unpack) ua
