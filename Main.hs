
module Main where

import qualified OutgoingWebhook as O
import qualified Data.Map as M
import Data.IORef (newIORef)

main :: IO()
main = do
  foo <- newIORef M.empty
  O.listen foo
