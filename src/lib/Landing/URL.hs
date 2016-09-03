{-# LANGUAGE OverloadedStrings #-}

module Landing.URL (index) where

import Data.String (IsString)

index :: IsString url => url
index = "/"
