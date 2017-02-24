{-# LANGUAGE OverloadedStrings #-}

module Session.URL (newRoute
                  , createRoute
                  , deleteRoute
                  , new
                  , create
                  , delete) where

import Data.String (IsString, fromString)

newRoute :: IsString url => url
newRoute = new

createRoute :: IsString url => url
createRoute = create

deleteRoute :: IsString url => url
deleteRoute = delete

new :: IsString url => url
new = "/session/new"

create :: IsString url => url
create = "/session"

delete :: IsString url => url
delete = "/session/delete"
