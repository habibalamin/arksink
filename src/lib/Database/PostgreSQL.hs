module Database.PostgreSQL (connection, withConnection) where

import System.Environment (getEnv)
import System.Posix.Env (getEnvDefault)
import Database.PostgreSQL.Simple (connect, defaultConnectInfo, Connection(..), ConnectInfo(..))

connection :: IO Connection
connection = do
  host     <- getEnvDefault "ARKSINK_DB_HOST" "localhost"
  username <- getEnvDefault "ARKSINK_DB_USERNAME" "postgres"
  password <- getEnv "ARKSINK_DB_PASSWORD"
  database <- getEnvDefault "ARKSINK_DB_NAME" "arksink"

  connect defaultConnectInfo { connectHost = host
                             , connectUser = username
                             , connectPassword = password
                             , connectDatabase = database }

withConnection :: (Connection -> IO a) -> IO a
withConnection = (connection >>=)
