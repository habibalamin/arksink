{-# LANGUAGE OverloadedStrings #-}

module Client.URL (newRoute
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
deleteRoute = delete ":id"

new :: IsString url => url
new = "/clients/new"

create :: IsString url => url
create = "/clients"

delete :: IsString url => String -> url
delete clientId = fromString $ "/clients/" ++ clientId
