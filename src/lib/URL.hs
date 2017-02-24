{-# LANGUAGE OverloadedStrings #-}

module URL (source
          , nugget
          , jbpVideos) where

import Data.String (IsString, fromString)

source :: IsString url => url
source = "http://code.alaminium.me/habibalamin/arksink.git"

nugget :: IsString url => url
nugget = "https://nugget.one"

jbpVideos :: IsString url => url
jbpVideos = "https://www.youtube.com/user/JordanPetersonVideos"
