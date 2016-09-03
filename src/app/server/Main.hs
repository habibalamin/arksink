module Main (main) where

import qualified Arksink.Server as Arksink.Server

main :: IO ()
main = Arksink.Server.run 3000
