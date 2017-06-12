module Arksink.Server (run) where

import qualified Configuration.Dotenv as Dotenv
import System.Posix.Env (setEnv, getEnvDefault)
import Web.Scotty (scotty, middleware, notFound)
import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import Network.Wai.Middleware.Static (initCaching,
                                      CachingStrategy(..),
                                      CacheContainer(..),
                                      staticPolicy',
                                      addBase)
import Cookie.Secure.Middleware (secureCookies)
import Network.Wai (Middleware)

import qualified Landing as Landing
import qualified Client as Client
import qualified Session as Session
import qualified Bookmark as Bookmark
import qualified General.View as General.View

data Environment
  = Development
  | Test
  | Production
  | Staging
  deriving (Show, Eq)

run :: Int -> IO ()
run port = do
  loadEnvFile
  environment <- environmentFromString
    <$> getEnvDefault "ARKSINK_ENV" "development"
  cacheContainer <- initCaching PublicStaticCaching
  server port environment cacheContainer

environmentFromString "production" = Production
environmentFromString "staging" = Staging
environmentFromString "test" = Test
environmentFromString _ = Development

loadEnvFile = Dotenv.parseFile ".env" >>= mapM_ setEnvTuple
  where
    setEnvTuple = ($ True) . flip . uncurry $ setEnv

server :: Int -> Environment -> CacheContainer -> IO ()
server port environment cacheContainer = scotty port $ do
  if environment == Development || environment == Test
  then middleware logStdoutDev
  else middleware logStdout
  middleware $ static cacheContainer
  middleware secureCookies

  Landing.routes
  Client.routes
  Session.routes
  Bookmark.routes

  notFound General.View.notFound

static :: CacheContainer -> Middleware
static cacheContainer = staticPolicy' cacheContainer $ addBase "public"
