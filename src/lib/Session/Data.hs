{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Session.Data (getSessionId
                   , createSession
                   , deleteCurrentSession
                   , addSession
                   , setSessionCookie) where

import Web.Scotty (ActionM, liftAndCatchIO)
import Control.Type.Operator (type ($))
import qualified Data.Text as Text (unpack)
import Web.Scotty.Cookie (getCookie, setCookie, deleteCookie, makeSimpleCookie)
import Data.String (IsString, fromString)
import Database.PostgreSQL.Simple.ToField (ToField)
import Data.ByteString (ByteString(..))
import Database.Redis (runRedis, hsetnx, hdel)
import qualified Data.ByteString.Char8 as BS (unpack)
import Data.Text (Text(..))
import Data.Time.Clock (getCurrentTime, addUTCTime)
import Web.Cookie (SetCookie(..))
import System.Random (randomRIO)
import Data.Char (intToDigit)

import Client.Data (Client(..), getClientFromEmail, validateClientPassword)
import qualified Database.RedisDB as Redis (connection)

getSessionId :: IsString id => ActionM $ Maybe id
getSessionId = fmap toStringy <$> getCookie "SessionId"
  where
    toStringy = fromString . Text.unpack

createSession :: (IsString email, ToField email) => email -> ByteString -> ActionM $ Either String ()
createSession email password = do
  maybeClient <- liftAndCatchIO $ getClientFromEmail email
  case maybeClient of
    Nothing -> return $ Left "Couldn't find client."
    Just client ->
      if validateClientPassword password client
      then fmap Right $ addSession client >>= setSessionCookie
      else return $ Left "Incorrect email address or password."

addSession :: IsString stringy => Client -> ActionM stringy
addSession client = liftAndCatchIO $ do
  sessionId <- generateSessionId
  c <- Redis.connection
  runRedis c . hsetnx "sessions" sessionId .
    fromString . show . clientId $ client
  return . fromString . BS.unpack $ sessionId

setSessionCookie :: Text -> ActionM ()
setSessionCookie sessionId = do
  expires <- liftAndCatchIO $ addUTCTime (86400 * 365) <$> getCurrentTime
  setCookie sessionCookie { setCookieHttpOnly = True
                          , setCookieSecure = True
                          , setCookieExpires = Just expires }
    where
      sessionCookie = makeSimpleCookie "SessionId" sessionId

deleteCurrentSession :: ActionM ()
deleteCurrentSession = do
  clearSession
  deleteCookie "SessionId"

clearSession :: ActionM ()
clearSession = do
  c <- liftAndCatchIO Redis.connection
  possibleSessionId <- getSessionId
  case possibleSessionId of
    Nothing -> return ()
    Just sessionId -> do
      liftAndCatchIO . runRedis c . hdel "sessions" $ [sessionId]
      return ()

generateSessionId :: IsString stringy => IO stringy
generateSessionId = do
  fmap fromString $ map intToDigit <$> digits
    where
      digits = sequence . replicate 10 $ randomRIO (0, 15)
