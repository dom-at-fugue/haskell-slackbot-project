
module Main where

import           Data.IORef      (newIORef)
import qualified Data.Map        as M
import qualified OutgoingWebhook as O

main :: IO()
main = do
  foo <- newIORef M.empty
  O.listen foo
