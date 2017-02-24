module Arksink.Server (run) where

import qualified Configuration.Dotenv as Dotenv
import System.Posix.Env (setEnv)
import Web.Scotty (scotty, middleware)
import Network.Wai.Middleware.Static (initCaching,
                                      CachingStrategy(..),
                                      CacheContainer(..),
                                      staticPolicy',
                                      addBase)
import Network.Wai (Middleware)

import qualified Landing as Landing

run :: Int -> IO ()
run port = do
    loadEnvFile
    server port =<< initCaching PublicStaticCaching

loadEnvFile = Dotenv.parseFile ".env" >>= mapM_ setEnvTuple
    where
        setEnvTuple = ($ True) . flip . uncurry $ setEnv

server :: Int -> CacheContainer -> IO ()
server port cacheContainer = scotty port $ do
    middleware $ static cacheContainer

    Landing.routes

static :: CacheContainer -> Middleware
static cacheContainer = staticPolicy' cacheContainer $ addBase "public"
