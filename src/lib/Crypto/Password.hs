{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Crypto.Password (hashPassword, validatePassword) where

import Data.String (IsString(..), fromString)
import Control.Type.Operator (type ($))
import Crypto.BCrypt (HashingPolicy(..)
                    , hashPasswordUsingPolicy
                    , slowerBcryptHashingPolicy)
import Data.ByteString (ByteString(..))
import Control.Monad (join)
import qualified Data.ByteString.Char8 as BS (unpack)
import qualified Crypto.BCrypt as BCrypt (validatePassword)
import System.Environment (lookupEnv)
import Data.Maybe (maybe)

import Extension.Functor ((<&>))
import Extension.Applicative ((<#>))

hashPassword :: IsString stringy => ByteString -> IO $ Maybe stringy
hashPassword password = hashedPassword <&> fmap fromByteString
    where
        hashedPassword = join $ hasher <#> password
        hasher = hashPasswordUsingPolicy <$> appHashingPolicy
        fromByteString = fromString . BS.unpack

validatePassword :: ByteString -> ByteString -> Bool
validatePassword = BCrypt.validatePassword

appHashingPolicy :: IO HashingPolicy
appHashingPolicy = do
    lookupEnv "ARKSINK_BCRYPT_COST" >>= \cost ->
        return slowerBcryptHashingPolicy { preferredHashCost = maybe 14 read cost
                                         , preferredHashAlgorithm = "$2b$" }
