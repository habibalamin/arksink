{-# LANGUAGE OverloadedStrings #-}

module Bookmark.URL (indexRoute
                   , showRoute
                   , newRoute
                   , createRoute
                   , editRoute
                   , updateRoute
                   , replaceRoute
                   , deleteRoute
                   , index
                   , show
                   , new
                   , create
                   , edit
                   , update
                   , replace
                   , delete
                   , mineRoute
                   , mine) where

import Prelude hiding (show)
import Data.String (IsString, fromString)

indexRoute :: IsString url => url
indexRoute = index

showRoute :: IsString url => url
showRoute = show ":id"

newRoute :: IsString url => url
newRoute = new

createRoute :: IsString url => url
createRoute = create

editRoute :: IsString url => url
editRoute = edit ":id"

updateRoute :: IsString url => url
updateRoute = update ":id"

replaceRoute :: IsString url => url
replaceRoute = replace ":id"

deleteRoute :: IsString url => url
deleteRoute = delete ":id"

index :: IsString url => url
index = "/bookmarks"

show :: IsString url => String -> url
show bookmarkId = fromString $ "/bookmarks/" ++ bookmarkId

new :: IsString url => url
new = "/bookmarks/new"

create :: IsString url => url
create = "/bookmarks"

edit :: IsString url => String -> url
edit bookmarkId = fromString $ "/bookmarks/" ++ bookmarkId ++ "/edit"

update :: IsString url => String -> url
update bookmarkId = fromString $ "/bookmarks/" ++ bookmarkId

replace :: IsString url => String -> url
replace bookmarkId = fromString $ "/bookmarks/" ++ bookmarkId

delete :: IsString url => String -> url
delete bookmarkId = fromString $ "/bookmarks/" ++ bookmarkId

-- UnRESTful routes

mineRoute :: IsString url => url
mineRoute = "/my-bookmarks"

mine :: IsString url => url
mine = mineRoute
