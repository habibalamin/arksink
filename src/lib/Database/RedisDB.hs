module Database.RedisDB (connection) where

import System.Environment (lookupEnv)
import Database.Redis (checkedConnect, defaultConnectInfo, Connection(..), ConnectInfo(..))
import Data.Maybe (fromMaybe)

connection :: IO Connection
connection = do
  db <- lookupEnv "ARKSINK_REDIS_DB"
  host <- lookupEnv "ARKSINK_REDIS_HOST"

  checkedConnect
    defaultConnectInfo
      { connectDatabase = maybe 0 read db
      , connectHost = fromMaybe (connectHost defaultConnectInfo) host
      }
