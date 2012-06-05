import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B
import Data.Binary.Get
import Data.Word
import Data.Bits

import Crypto

data Header = Header {
    headerSize :: Word,
    archiveDataLength :: Word,
    blockSize :: Word,
    hashTableLength :: Word,
    hashTableOffset :: Word,
    hashTableCompressedSize :: Word,
    blockTableLength :: Word,
    blockTableOffset :: Word,
    blockTableCompressedSize :: Word,
    highBlockTableOffset :: Word,
    highBlockTableCompressedSize :: Word
} deriving (Show)

-- TODO: handle MPQ Data in middle
archiveDataOffset = 0

parseHeader :: Get Header
parseHeader = do
    signature <- getWord32le >>= return . fromIntegral
    -- TODO: check for MPQ user data presence

    headerSize <- getWord32le >>= return . fromIntegral
    archiveDataLengthV1 <- getWord32le >>= return . fromIntegral
    mpqVersion <- getWord16le >>= return . fromIntegral
    blockSize <- getWord16le >>= return . (shift 0x200) . fromIntegral
    hashTableOffsetV1 <- getWord32le >>= return . fromIntegral
    blockTableOffsetV1 <- getWord32le >>= return . fromIntegral
    hashTableLength <- getWord32le >>= return . fromIntegral
    blockTableLength <- getWord32le >>= return . fromIntegral

    let hashTableCompressedSize = let delta = blockTableOffsetV1 - hashTableOffsetV1
                                      size = 16 * hashTableLength
               in if (delta > 0 && delta < size) then delta else size
    let blockTableCompressedSize = 16 * blockTableLength

    (hashTableOffsetV2, blockTableOffsetV2, highBlockTableOffset, highBlockTableCompressedSize) <- if mpqVersion >= 1
        then parseV2 hashTableOffsetV1 blockTableOffsetV1 blockTableLength
        else return (hashTableOffsetV1, blockTableOffsetV1, 0, 0)

    archiveDataLengthV3 <- if mpqVersion >= 2 && headerSize >= 0x44
        then parseV3
        else return (if highBlockTableOffset > hashTableOffsetV2
            then if highBlockTableOffset > blockTableOffsetV2
                then highBlockTableOffset + 4 * blockTableLength
                else blockTableOffsetV2 + 16 * blockTableLength
            else hashTableOffsetV2 + 16 * hashTableLength)
    
    (hashTableCompressedSizeV4, blockTableCompressedSizeV4, highBlockTableCompressedSize) <- if mpqVersion >= 3
        then parseV4
        else return (hashTableCompressedSize, blockTableCompressedSize, if highBlockTableOffset > 0 then 4 * blockTableLength else 0)

    let hashTableSize = 16 * hashTableLength
    let blockTableSize = 16 * blockTableLength
    let highBlockTableSize = if highBlockTableOffset /= 0 then 4 * blockTableLength else 0

    -- TODO: check for strong signature presence

    return $ Header headerSize archiveDataLengthV3 blockSize hashTableLength hashTableOffsetV2 hashTableCompressedSizeV4 blockTableLength blockTableOffsetV2 blockTableCompressedSize highBlockTableOffset highBlockTableCompressedSize

parseV2 :: Word -> Word -> Word -> Get (Word, Word, Word, Word)
parseV2 hashTableOffsetV1 blockTableOffsetV1 blockTableLength = do
    highBlockTableOffset <- getWord64le >>= return . fromIntegral
    let highBlockTableCompressedSize = if highBlockTableOffset /= 0 then 4 * blockTableLength else 0
    hashTableOffsetHigh <- getWord16le >>= return . fromIntegral
    blockTableOffsetHigh <- getWord16le >>= return . fromIntegral
    let hashTableOffsetV2 = hashTableOffsetV1 `xor` (hashTableOffsetHigh `shift` 32)
    let blockTableOffsetV2 = blockTableOffsetV1 `xor` (blockTableOffsetHigh `shift` 32)

    return (hashTableOffsetV2, blockTableOffsetV2, highBlockTableOffset, highBlockTableCompressedSize)

parseV3 :: Get Word
parseV3 = do
    archiveDataLength <- getWord64le >>= return . fromIntegral
    skip 8 -- enhancedBlockTableOffset
    skip 8 -- enhancedHashTableOffset
    return archiveDataLength

parseV4 :: Get (Word, Word, Word)
parseV4 = do
    hashTableCompressedSize <- getWord64le >>= return . fromIntegral
    blockTableCompressedSize <- getWord64le >>= return . fromIntegral
    highBlockTableCompressedSize <- getWord64le >>= return . fromIntegral
    skip 8 -- enhancedHashTableCompressedSize
    skip 8 -- enhancedBlockTableCompressedSize
    skip 4 -- rawChunkSize
    skip $ 6 * 16 -- hashes
    return (hashTableCompressedSize, blockTableCompressedSize, highBlockTableCompressedSize)

parseHashTable :: Header -> Get [ Word ]
parseHashTable header = do
    skip $ archiveDataOffset + (fromIntegral $ hashTableOffset header)
    ret <- parseEncryptedUInt32Table (hashTableLength header) (hashTableCompressedSize header) (hash "(hash table)" 0x300)
    return ret

parseEncryptedUInt32Table :: Word -> Word -> Word -> Get [ Word ]
parseEncryptedUInt32Table tableLength dataLength hashValue = do
    let uintCount = tableLength `shiftL` 2
    -- TODO: support compressed
    bytes <- getByteString $ fromIntegral dataLength 
    -- TODO: handle big endian
    return $ decrypt hashValue $ cs2ws $ B.unpack bytes
        where cs2ws [] = []
              cs2ws (a:b:c:d:rest) = (fromIntegral a .|. (fromIntegral b `shift` 8) .|. (fromIntegral c `shift` 16) .|. (fromIntegral d `shift` 24)) : (cs2ws rest)

parseFile :: String -> IO ()
parseFile path = do
    contents <- L.readFile path
    let header = runGet parseHeader contents
    print header
    print $ runGet (parseHashTable header) contents 

main = parseFile "test.mpq"
