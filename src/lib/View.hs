{-# LANGUAGE OverloadedStrings #-}

module View (layout, script, stylesheet) where

import Web.Scotty (ActionM)
import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import SessionClient.Data (getCurrentClient, clientCase)
import qualified Bookmark.URL as Bookmark.URL
import qualified Session.URL as Session.URL
import qualified URL as URL

layout :: Html -> Html -> ActionM Html
layout head_ body = do
  currentClient <- getCurrentClient

  return . H.docTypeHtml $ do
    H.head $ do
      H.meta ! A.name "charset" ! A.content "UTF-8"
      H.title "Arksink"

      stylesheet "/assets/styles/arksink.css"
      script "/assets/scripts/arksink.elm.js"
      script "/assets/scripts/arksink.js"

      head_

    H.body $ do
      H.header $ do
        H.h1 "Arksink"
        H.h2 "Bookmark management for the indecisive"
        H.nav $ do
          H.ul $ do
            linkItem "My Bookmarks" Bookmark.URL.mine
            linkItem "Discover" Bookmark.URL.index
            clientCase currentClient
              (linkItem "Sign in" Session.URL.new ! A.id "login")
              (\_ -> linkItem "Sign out" Session.URL.delete ! A.id "logout")

      H.main body

      H.footer $ do
        H.section ! A.id "useful-links" $ do
          H.h1 "Useful Links"
          H.ul $ do
            linkItem "About" "/about"
            linkItem "Support" "/support"
            linkItem "Source" URL.source
        H.section ! A.id "copyright" $ "© حبيب الامين \x202A\&2017"
        H.section ! A.id "editors-picks" $ do
          H.h1 "Editor's Picks"
          H.ul $ do
            linkItem "Nugget" URL.nugget
            linkItem "Jordan B Peterson" URL.jbpVideos

linkItem text href = H.li $ H.a ! A.href href $ text

stylesheet href = H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href href

script src = H.script ! A.src src $ ""
