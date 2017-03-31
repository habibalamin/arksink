module Database.RedisDB (connection) where

import System.Environment (lookupEnv)
import Database.Redis (checkedConnect, defaultConnectInfo, Connection(..), ConnectInfo(..))
import Data.Maybe (fromMaybe)

connection :: IO Connection
connection = lookupEnv "ARKSINK_REDIS_DB" >>= \db ->
  checkedConnect defaultConnectInfo { connectDatabase = fromMaybe 0 $ read <$> db }
