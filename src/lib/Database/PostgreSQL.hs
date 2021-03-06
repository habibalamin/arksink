module Database.PostgreSQL (connection) where

import System.Environment (getEnv)
import Database.PostgreSQL.Simple (connect, defaultConnectInfo, Connection(..), ConnectInfo(..))

connection :: IO Connection
connection = do
  username <- getEnv "ARKSINK_DB_USERNAME"
  password <- getEnv "ARKSINK_DB_PASSWORD"
  connect defaultConnectInfo { connectUser = username
                             , connectPassword = password
                             , connectDatabase = "arksink" }
