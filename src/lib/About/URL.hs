{-# LANGUAGE OverloadedStrings #-}

module About.URL (indexRoute
                , index) where

import Data.String (IsString)

indexRoute :: IsString url => url
indexRoute = index

index :: IsString url => url
index = "/about"
