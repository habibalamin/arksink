{-# LANGUAGE OverloadedStrings #-}

module Bookmark.View (index, show, new) where

import Prelude hiding (show)
import qualified Prelude
import Text.Blaze.Html5 (Html, (!))
import Web.Scotty (ActionM)
import Control.Monad (forM_)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Bookmark.Data (Bookmark(..), bookmarkId, bookmarkTitle, bookmarkURL)
import qualified Bookmark.URL as Bookmark.URL
import View (layout)

instance H.ToMarkup Bookmark where
  toMarkup bookmark = do
    H.article ! A.class_ "bookmark" $ do
      H.h1 $ do
        H.span ! A.class_ "permalink" $ do
          H.a ! A.href internalURL $ "★"
        " "
        H.a ! A.href externalURL $ H.toHtml title
      where
        internalURL = Bookmark.URL.show . Prelude.show $ bookmarkId bookmark
        externalURL = H.toValue $ bookmarkURL bookmark
        title = bookmarkTitle bookmark

index :: [Bookmark] -> ActionM Html
index bookmarks = layout "" $ do
  H.h1 "Bookmarks"

  if null bookmarks then do
    H.h2 "Oh… looks like you don't have any yet!"
    H.a ! A.href Bookmark.URL.new $ "Create a bookmark?"
  else do
    H.section ! A.class_ "actions" $ do
      H.a ! A.href Bookmark.URL.new $ "New bookmark"
    H.section ! A.id "bookmarks" $ do
      forM_ bookmarks H.toHtml

show :: Bookmark -> ActionM Html
show bookmark = layout "" $ H.toHtml bookmark

new :: ActionM Html
new = layout "" $ do
  H.h1 "New bookmark"

  H.form ! A.action Bookmark.URL.createRoute ! A.method "POST" $ do
    H.label "Title" ! A.for "title" >> H.input ! A.name "title"
    H.label "URL" ! A.for "url" >> H.input ! A.name "url"
    H.button "Create bookmark"
