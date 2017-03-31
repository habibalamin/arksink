{-# LANGUAGE OverloadedStrings #-}

module General.View (notFound) where

import Web.Scotty (ActionM, status)
import Network.HTTP.Types.Status (notFound404)

import Static (staticPage)

notFound :: ActionM ()
notFound = do
  status notFound404
  staticPage "static/404.html"
