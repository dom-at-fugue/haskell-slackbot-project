
module Main where

import qualified OutgoingWebhook as O
import Data.Map
import Data.IORef (IORef, newIORef, atomicModifyIORef)
import Debug.Trace

import Control.Monad.Trans (liftIO, lift)

main :: IO()
main = O.listen
