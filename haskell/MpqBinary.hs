module MpqBinary where

import Data.Word (Word16, Word32, Word64)
import Data.Binary.Get
import Control.Applicative ((<$>))

class MpqBinary a where
    getMB :: Get a

instance MpqBinary Word16 where
    getMB = fromIntegral <$> getWord16le

instance MpqBinary Word32 where
    getMB = fromIntegral <$> getWord32le

instance MpqBinary Word64 where
    getMB = fromIntegral <$> getWord64le
