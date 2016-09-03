module Landing (routes) where

import Web.Scotty (ScottyM, get, html)
import Text.Blaze.Html.Renderer.Text (renderHtml)

import qualified Landing.URL as Landing.URL
import qualified Landing.View as Landing.View

routes :: ScottyM ()
routes = get Landing.URL.index $ html . renderHtml $ Landing.View.index
