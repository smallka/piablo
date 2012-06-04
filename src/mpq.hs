import qualified Data.ByteString.Lazy as L
import Data.Binary.Get
import Data.Word
import Data.Bits

data Header = Header {
    headerSize :: Int,

    archiveDataLength :: Int,
    blockSize :: Int
} deriving (Show)

parseHeader :: Get Header
parseHeader = do
    signature <- getWord32le >>= return . fromIntegral
    let archiveDataOffset = 0

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
    
    readHashTable hashTableLength hashTableOffsetV2 hashTableCompressedSizeV4

    return $ Header headerSize archiveDataLengthV3 blockSize

parseV2 :: Int -> Int -> Int -> Get (Int, Int, Int, Int)
parseV2 hashTableOffsetV1 blockTableOffsetV1 blockTableLength = do
    highBlockTableOffset <- getWord64le >>= return . fromIntegral
    let highBlockTableCompressedSize = if highBlockTableOffset /= 0 then 4 * blockTableLength else 0
    hashTableOffsetHigh <- getWord16le >>= return . fromIntegral
    blockTableOffsetHigh <- getWord16le >>= return . fromIntegral
    let hashTableOffsetV2 = hashTableOffsetV1 `xor` (hashTableOffsetHigh `shift` 32)
    let blockTableOffsetV2 = blockTableOffsetV1 `xor` (blockTableOffsetHigh `shift` 32)

    return (hashTableOffsetV2, blockTableOffsetV2, highBlockTableOffset, highBlockTableCompressedSize)

parseV3 :: Get Int
parseV3 = do
    archiveDataLength <- getWord64le >>= return . fromIntegral
    skip 8 -- enhancedBlockTableOffset
    skip 8 -- enhancedHashTableOffset
    return archiveDataLength

parseV4 :: Get (Int, Int, Int)
parseV4 = do
    hashTableCompressedSize <- getWord64le >>= return . fromIntegral
    blockTableCompressedSize <- getWord64le >>= return . fromIntegral
    highBlockTableCompressedSize <- getWord64le >>= return . fromIntegral
    skip 8 -- enhancedHashTableCompressedSize
    skip 8 -- enhancedBlockTableCompressedSize
    skip 4 -- rawChunkSize
    skip $ 6 * 16 -- hashes
    return (hashTableCompressedSize, blockTableCompressedSize, highBlockTableCompressedSize)

hash :: String -> Int -> Int
hash text hashOffset = 0

readHashTable :: Int -> Int -> Int -> Get ()
readHashTable hashTableLength hashTableOffset hashTableCompressedSize = do
    readEncryptedUInt32Table hashTableLength hashTableOffset hashTableCompressedSize (hash "(hash table)" 0x300)

readEncryptedUInt32Table :: Int -> Int -> Int -> Int -> Get ()
readEncryptedUInt32Table tableLength tableOffset dataLength hashValue = do
    return ()

parseFile path = do
    contents <- L.readFile path
    print $ runGet parseHeader contents
