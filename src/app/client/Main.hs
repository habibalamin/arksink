module Main (main) where

import System.Environment (getArgs)

import qualified Arksink.Client as Arksink.Client

main :: IO ()
main = Arksink.Client.run
