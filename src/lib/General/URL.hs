{-# LANGUAGE OverloadedStrings #-}

module General.URL (rootRoute, root) where

import Data.String (IsString)

rootRoute :: IsString url => url
rootRoute = root

root :: IsString url => url
root = "/"
