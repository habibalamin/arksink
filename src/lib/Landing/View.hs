{-# LANGUAGE OverloadedStrings #-}

module Landing.View (index) where

import Web.Scotty (ActionM)
import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import View (layout)

index :: ActionM Html
index = layout "" ""
