{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module RequestInfo (relativeReferer
                  , relativeRefererOrRoot) where

import Data.String (IsString, fromString)
import Web.Scotty (ActionM, header)
import Data.Maybe (fromMaybe, maybe)
import Control.Type.Operator (type ($))
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Applicative ((<|>))
import Control.Monad (guard)
import Network.URI (nullURI
                  , uriIsRelative
                  , uriPath
                  , uriRegName
                  , uriAuthority
                  , parseURI)
import qualified Data.Text.Lazy as Text (unpack)

import qualified General.URL as General.URL

relativeRefererOrRoot :: IsString path => ActionM path
relativeRefererOrRoot = fromMaybe General.URL.root <$> relativeReferer

relativeReferer :: IsString referer => ActionM $ Maybe referer
relativeReferer = runMaybeT $ do
    hostUrl    <- MaybeT (maybeUri <$> header "Host"   ) <|> return nullURI
    refererUrl <- MaybeT (maybeUri <$> header "Referer")

    guard $ uriIsRelative refererUrl || domain refererUrl == domain hostUrl

    return . fromString . uriPath $ refererUrl
  where
    domain = fmap uriRegName . uriAuthority
    maybeUri = maybe Nothing $ parseURI . Text.unpack
