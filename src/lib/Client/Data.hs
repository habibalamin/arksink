{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Client.Data (Client(..)
                  , getClients
                  , getClient
                  , getClientFromEmail
                  , createClient
                  , deleteClient
                  , validateClientPassword) where

import Prelude hiding (head)
import qualified Prelude as P
import Protolude (head)
import Data.String (IsString(..), fromString)
import Database.PostgreSQL.Simple (query, query_, Only(..), execute)
import Database.PostgreSQL.Simple.FromRow (FromRow(..), field)
import Database.PostgreSQL.Simple.ToField (ToField)
import Control.Type.Operator (type ($))
import Data.ByteString (ByteString(..))

import qualified Database.PostgreSQL as PG (connection)
import Crypto.Password (hashPassword, validatePassword)

data Client = Client { clientId :: Int
                     , clientName :: Maybe String
                     , clientEmail :: String
                     , clientPasswordHash :: String
                     } deriving Show

instance FromRow Client where
  fromRow = Client <$> field <*> field <*> field <*> field

getClients :: IO [Client]
getClients = PG.connection >>= flip query_ "SELECT id, name, email_address, password_hash FROM clients"

getClientFromEmail :: ToField email => email -> IO $ Maybe Client
getClientFromEmail email = do
  c <- PG.connection
  head <$> query c
    "SELECT id, name, email_address, password_hash FROM clients WHERE email_address = ? LIMIT 1"
    [email]

getClient :: ToField id => id -> IO $ Maybe Client
getClient clientId = do
  c <- PG.connection
  head <$> query c
    "SELECT id, name, email_address, password_hash FROM clients WHERE id = ? LIMIT 1"
    [clientId]

-- OPTIMIZE: raelly unfortunate top-level type signature
createClient :: (IsString flike, ToField flike, IsString id) => flike -> ByteString -> IO $ Maybe id
createClient email password = do
  maybePasswordHash <- hashPassword password
  case maybePasswordHash of
    Nothing -> return Nothing
    -- Polymorphic string value of the first field of the first (i.e. inserted) row
    Just passwordHash -> return . fromString . show . P.head . P.head <$> idInRow where
      idInRow = PG.connection >>= \c -> query c sqlString [email, passwordHash] :: IO [[Int]]
      sqlString = "INSERT INTO clients (email_address, password_hash) VALUES (?, ?) RETURNING id"

deleteClient :: ToField id => id -> IO ()
deleteClient clientId = do
  c <- PG.connection
  execute c "DELETE FROM clients WHERE id = ?" $ Only clientId
  return ()

validateClientPassword :: ByteString -> Client -> Bool
validateClientPassword password =
  flip validatePassword password . fromString . clientPasswordHash
