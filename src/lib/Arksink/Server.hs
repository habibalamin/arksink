module Arksink.Server (run) where

import Web.Scotty (scotty)
import qualified Configuration.Dotenv as Dotenv
import System.Posix.Env (setEnv)

import qualified Landing as Landing

run :: Int -> IO ()
run port = do
    loadEnvFile
    scotty port $ do
        Landing.routes

loadEnvFile = Dotenv.parseFile ".env" >>= mapM_ setEnvTuple
    where
        setEnvTuple = ($ True) . flip . uncurry $ setEnv
