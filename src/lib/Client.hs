{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Client (routes) where

import Web.Scotty (ScottyM, get, post, redirect, html, param, liftAndCatchIO)
import Data.ByteString (ByteString(..))
import Text.Blaze.Html.Renderer.Text (renderHtml)

import qualified Client.URL as Client.URL
import qualified Client.View as Client.View
import Client.Data (createClient)
import Session.Data (createSession)
import RequestInfo (relativeRefererOrRoot)

routes :: ScottyM ()
routes = do
  get Client.URL.newRoute $ do
    Client.View.new >>= html . renderHtml

  post Client.URL.createRoute $ do
    email_address ::     String <- param "email_address"
    password      :: ByteString <- param "password"

    liftAndCatchIO $ createClient email_address password
    createSession email_address password

    redirect =<< relativeRefererOrRoot
