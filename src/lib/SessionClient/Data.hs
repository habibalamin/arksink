{-# LANGUAGE OverloadedStrings #-}

module SessionClient.Data (SessionClient(..)
                         , getCurrentClient
                         , clientCase) where

import Web.Scotty (ActionM, liftAndCatchIO)
import Data.ByteString (ByteString(..))
import Database.Redis (runRedis, hget)
import Control.Monad (join)
import Data.Maybe (maybe)

import Client.Data (Client(..), getClient)
import Session.Data (getSessionId)
import qualified Database.RedisDB as Redis (connection)

data SessionClient = Guest | SignedIn Client deriving Show

getCurrentClient :: ActionM SessionClient
getCurrentClient = do
    possibleSessionId <- getSessionId
    case possibleSessionId of
        Nothing -> return Guest
        Just sessionId -> liftAndCatchIO $ getClientFromSessionId sessionId

isSignedIn :: SessionClient -> Bool
isSignedIn = not . isGuest

isGuest :: SessionClient -> Bool
isGuest Guest = True
isGuest _     = False

clientCase :: SessionClient -> a -> (SessionClient -> a) -> a
clientCase client a b = if isGuest client then a else b client

getClientFromSessionId :: ByteString -> IO SessionClient
getClientFromSessionId sessionId = do
    c <- Redis.connection
    redisReply <- runRedis c . hget "sessions" $ sessionId
    -- OPTIMIZE: WTF is this leaning shit?
    case redisReply of
        Left reply -> error $ show reply
        Right possibleUserId -> do
            case possibleUserId of
                Nothing -> return Guest
                Just userId -> do
                    possibleClient <- getClient userId
                    case possibleClient of
                        Nothing -> return Guest
                        Just client -> return $ SignedIn client
