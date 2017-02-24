module Static (staticPage) where

import Prelude hiding (readFile)
import qualified Data.Text.Lazy.IO as TextIO
import Web.Scotty.Trans (liftAndCatchIO)
import Web.Scotty (ActionM, html)
import Data.Text.Lazy (Text)

staticPage :: FilePath -> ActionM ()
staticPage path = readFile path >>= html

readFile :: FilePath -> ActionM Text
readFile = liftAndCatchIO . TextIO.readFile
