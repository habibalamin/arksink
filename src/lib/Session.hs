{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Session (routes) where

import Web.Scotty (ScottyM, get, post, delete, redirect, html, text, param)
import Data.ByteString (ByteString(..))
import Text.Blaze.Html.Renderer.Text (renderHtml)

import qualified Session.URL as Session.URL
import qualified Session.View as Session.View
import Session.Data (createSession, deleteCurrentSession)
import RequestInfo (relativeRefererOrRoot)

routes :: ScottyM ()
routes = do
  get Session.URL.newRoute $ do
    Session.View.new >>= html . renderHtml

  post Session.URL.createRoute $ do
    email_address ::     String <- param "email_address"
    password      :: ByteString <- param "password"
    createSession email_address password >>= \result ->
      case result of
        -- TODO: display error message for failure reason.
        Left error -> Session.View.new >>= html . renderHtml
        Right _ -> redirect =<< relativeRefererOrRoot

  delete Session.URL.deleteRoute $ do
    deleteCurrentSession
    text =<< relativeRefererOrRoot
