{-# LANGUAGE OverloadedStrings #-}

module General.View (notFound, forbidden) where

import Web.Scotty (ActionM, status)
import Network.HTTP.Types.Status (notFound404, forbidden403)

import Static (staticPage)

notFound :: ActionM ()
notFound = do
  status notFound404
  staticPage "static/404.html"

forbidden :: ActionM ()
forbidden = do
  status forbidden403
  staticPage "static/403.html"
