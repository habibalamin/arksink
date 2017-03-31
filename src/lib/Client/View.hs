{-# LANGUAGE OverloadedStrings #-}

module Client.View (new) where

import Web.Scotty (ActionM)
import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import qualified Client.URL as Client.URL
import View (layout)

new :: ActionM Html
new = layout "" $ do
    H.h1 "Sign up"

    H.form ! A.action Client.URL.createRoute ! A.method "POST" $ do
        H.label "Email address" >> H.input ! A.name "email_address"
        H.label "Password" >> H.input ! A.type_ "password" ! A.name "password"
        H.button "Sign up"
