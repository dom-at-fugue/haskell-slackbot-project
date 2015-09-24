module Main where

--import IncomingWebhook
import qualified OutgoingWebhook as O
import Data.Map
import Data.IORef (IORef, newIORef, atomicModifyIORef)
import Debug.Trace

import Control.Monad.Trans (liftIO, lift)

main :: IO()
main = do
  afkMap <- newIORef empty
  O.listen afkMap
