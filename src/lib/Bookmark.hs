{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bookmark (routes) where

import Web.Scotty (ScottyM, get, post, patch, put, delete, redirect, text, html, param)
import Web.Scotty.Trans (liftAndCatchIO)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Data.Maybe (maybe)

import Bookmark.Data (getBookmarks, getBookmark, createBookmark, deleteBookmark)
import qualified Bookmark.URL as Bookmark.URL
import qualified Bookmark.View as Bookmark.View
import qualified General.View as General.View

routes :: ScottyM ()
routes = do
    get Bookmark.URL.indexRoute $ do
        bookmarks <- liftAndCatchIO getBookmarks
        html . renderHtml . Bookmark.View.index $ bookmarks

    get Bookmark.URL.showRoute $ do
        bookmarkId :: Int <- param "id"
        bookmark <- liftAndCatchIO $ getBookmark bookmarkId
        let showBookmark = html . renderHtml . Bookmark.View.show
            in maybe General.View.notFound showBookmark bookmark

    get Bookmark.URL.newRoute $ do
        html . renderHtml $ Bookmark.View.new

    post Bookmark.URL.createRoute $ do
        title :: String <- param "title"
        url :: String <- param "url"
        bookmarkId <- liftAndCatchIO $ createBookmark title url
        redirect $ Bookmark.URL.show bookmarkId

    get Bookmark.URL.editRoute $ do
        text "Edit bookmark"

    patch Bookmark.URL.updateRoute $ do
        text "Update bookmark"

    put Bookmark.URL.replaceRoute $ do
        text "Replace bookmark"

    delete Bookmark.URL.deleteRoute $ do
        bookmarkId :: Int <- param "id"
        liftAndCatchIO $ deleteBookmark bookmarkId
        redirect Bookmark.URL.index
