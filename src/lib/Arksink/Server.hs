module Arksink.Server (run) where

import Web.Scotty (scotty)

import qualified Landing as Landing

run :: Int -> IO ()
run port = scotty port $ do
    Landing.routes
