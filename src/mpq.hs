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
    headerSize <- getWord32le >>= return . fromIntegral
    adl <- getWord32le >>= return . fromIntegral
    mpqVersion <- getWord16le >>= return . fromIntegral
    blockSize <- getWord16le >>= return . (shift 0x200) . fromIntegral
    hto <- getWord32le >>= return . fromIntegral
    bto <- getWord32le >>= return . fromIntegral
    hashTableLength <- getWord32le >>= return . fromIntegral
    blockTableLength <- getWord32le >>= return . fromIntegral

    let htcs = let delta = bto - hto
                   size = 16 * hashTableLength
               in if (delta > 0 && delta < size) then delta else size
    let btcs = 16 * blockTableLength

    (hashTableOffset, blockTableOffset, highBlockTableOffset, highBlockTableCompressedSize) <- if mpqVersion >= 1
        then parseV2 hto bto blockTableLength
        else return (hto, bto, 0, 0)

    archiveDataLength <- if mpqVersion >= 2 && headerSize >= 0x44
        then parseV3
        else return (if highBlockTableOffset > hashTableOffset
            then if highBlockTableOffset > blockTableOffset
                then highBlockTableOffset + 4 * blockTableLength
                else blockTableOffset + 16 * blockTableLength
            else hashTableOffset + 16 * hashTableLength)
    
    (hashTableCompressedSize, blockTableCompressedSize, highBlockTableCompressedSize) <- if mpqVersion >= 3
        then parseV4
        else return (htcs, btcs, if highBlockTableOffset > 0 then 4 * blockTableLength else 0)

    return $ Header headerSize archiveDataLength blockSize

parseV2 :: Int -> Int -> Int -> Get (Int, Int, Int, Int)
parseV2 hto bto blockTableLength = do
    highBlockTableOffset <- getWord64le >>= return . fromIntegral
    let highBlockTableCompressedSize = if highBlockTableOffset /= 0 then 4 * blockTableLength else 0
    hashTableOffsetHigh <- getWord16le >>= return . fromIntegral
    blockTableOffsetHigh <- getWord16le >>= return . fromIntegral
    let hashTableOffset = hto `xor` (hashTableOffsetHigh `shift` 32)
    let blockTableOffset = bto `xor` (blockTableOffsetHigh `shift` 32)

    return (hashTableOffset, blockTableOffset, highBlockTableOffset, highBlockTableCompressedSize)

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
    return (hashTableCompressedSize, blockTableCompressedSize, highBlockTableCompressedSize)

parseFile path = do
    contents <- L.readFile path
    print $ runGet parseHeader contents
