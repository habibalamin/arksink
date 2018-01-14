{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Bookmark.Data (Bookmark(..)
                    , getBookmarks
                    , getBookmarksFromClient
                    , getBookmark
                    , createBookmark
                    , deleteBookmark) where

import Prelude hiding (head)
import qualified Prelude as P
import Protolude (head)
import Data.String (IsString(..), fromString)
import Database.PostgreSQL.Simple (query, query_, Only(..), execute)
import Database.PostgreSQL.Simple.FromRow (FromRow(..), field)
import Database.PostgreSQL.Simple.ToField (ToField)
import Client.Data (Client(..))
import Control.Type.Operator (type ($))

import qualified Database.PostgreSQL as PG (connection)

data Bookmark = Bookmark { bookmarkId :: Int
                         , bookmarkTitle :: String
                         , bookmarkURL :: String
                         } deriving Show

instance FromRow Bookmark where
  fromRow = Bookmark <$> field <*> field <*> field

getBookmarks :: IO [Bookmark]
getBookmarks = PG.connection >>= flip query_ "SELECT id, title, url FROM bookmarks"

getBookmarksFromClient :: Client -> IO [Bookmark]
getBookmarksFromClient client = do
    c <- PG.connection
    query c "SELECT id, title, url FROM bookmarks WHERE creator_id = ?"
      $ Only creatorId
      where
        creatorId = clientId client

getBookmark :: ToField id => id -> IO $ Maybe Bookmark
getBookmark bookmarkId = do
  c <- PG.connection
  head <$> query c "SELECT id, title, url FROM bookmarks WHERE id = ? LIMIT 1" [bookmarkId]

-- FIXME: I can't allow creatorId to be a different type from title & url.
createBookmark :: (ToField flike, IsString id) => flike -> flike -> flike -> IO id
createBookmark creatorId title url = do
  -- Polymorphic string value of the first field of the first (i.e. inserted) row
  fromString . show . P.head . P.head <$> idInRow where
    idInRow = PG.connection >>= \c -> query c sqlString [creatorId, title, url] :: IO [[Int]]
    sqlString = "INSERT INTO bookmarks (creator_id, title, url) VALUES (?, ?, ?) RETURNING id"

deleteBookmark :: ToField id => id -> IO ()
deleteBookmark bookmarkId = do
  c <- PG.connection
  execute c "DELETE FROM bookmarks WHERE id = ?" $ Only bookmarkId
  return ()
