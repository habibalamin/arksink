{-# LANGUAGE OverloadedStrings #-}

module Session.View (new) where

import Web.Scotty (ActionM)
import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import qualified Session.URL as Session.URL
import View (layout)

new :: ActionM Html
new = layout "" $ do
    H.h1 "Sign in"

    H.form ! A.action Session.URL.createRoute ! A.method "POST" $ do
        H.label "Email address" >> H.input ! A.name "email_address"
        H.label "Password" >> H.input ! A.type_ "password" ! A.name "password"
        H.button "Sign in"
