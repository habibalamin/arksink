{-# LANGUAGE OverloadedStrings #-}

module Landing.View (index) where

import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

index :: Html
index = H.docTypeHtml $ do
    H.head $ do
        H.title "Arksink"

    H.body $ do
        H.main $ do
            H.h1 "Arksink"
            H.h2 "A cloud bookmark synchroniser for the indecisive"
