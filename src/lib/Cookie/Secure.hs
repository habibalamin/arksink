module Cookie.Secure (encryptAndSign , verifyAndDecrypt) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Crypto.Error (CryptoFailable, maybeCryptoError)

import Crypto.Encryption (encrypt, decrypt)
import Crypto.Verification (sign
                          , verify
                          , serialize
                          , deserialize
                          , getSignable)

encryptAndSign
  :: String
  -> String
  -> String
  -> ByteString
  -> CryptoFailable ByteString
encryptAndSign iv encryptKey authKey message = serialize <$> signed
  where
    signed = sign authKey <$> encrypted
    encrypted = encrypt iv encryptKey message

-- OPTIMIZE: wrap result in Either errorType, instead of Maybe.
verifyAndDecrypt :: String -> String -> ByteString -> Maybe ByteString
verifyAndDecrypt authKey encryptKey message =
  deserialize message >>= verifyAndDecryptDeserialized
    where
      verifyAndDecryptDeserialized signed = 
        if verify authKey signed
        then maybeCryptoError $ decrypt encryptKey (getSignable signed)
        else Nothing
