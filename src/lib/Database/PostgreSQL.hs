module Database.PostgreSQL (connection, withConnection) where

import System.Environment (getEnv)
import Database.PostgreSQL.Simple (connect, defaultConnectInfo, Connection(..), ConnectInfo(..))

connection :: IO Connection
connection = do
  username <- getEnv "ARKSINK_DB_USERNAME"
  password <- getEnv "ARKSINK_DB_PASSWORD"
  database <- getEnv "ARKSINK_DB_NAME"
  connect defaultConnectInfo { connectUser = username
                             , connectPassword = password
                             , connectDatabase = database }

withConnection :: (Connection -> IO a) -> IO a
withConnection = (connection >>=)
