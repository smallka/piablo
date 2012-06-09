import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B
import Data.Binary.Get
import Data.Word
import Data.Bits
import Control.Monad (when)

import Crypto (hash, decrypt)

data MpqHeader = MpqHeader {
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

data MpqHashEntry = MpqHashEntry {
    hashA :: Word,
    hashB :: Word,
    locale :: Int,
    block :: Int
} deriving (Show)

isEntryInvalid :: MpqHashEntry -> Bool
isEntryInvalid entry = block entry == -1 || hashA entry == 0xFFFFFFFF || hashB entry == 0xFFFFFFFF

hashMatch :: MpqHashEntry -> String -> Bool
hashMatch entry filename = hashA entry == (hash filename 0x100) && hashB entry == (hash filename 0x200)

data MpqBlockEntry = MpqBlockEntry {
    offset :: Word,
    compressedSize :: Word,
    uncompressedSize :: Word,
    flags :: Word
} deriving (Show)

parseHeader :: Get MpqHeader
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

    hashTableCompressedSize <- return $
        let delta = blockTableOffsetV1 - hashTableOffsetV1
            size = 16 * hashTableLength
        in if (delta > 0 && delta < size)
            then delta
            else size
    let blockTableCompressedSize = 16 * blockTableLength

    (hashTableOffsetV2, blockTableOffsetV2, highBlockTableOffset, highBlockTableCompressedSize) <- if mpqVersion >= 1
        then parseV2 hashTableOffsetV1 blockTableOffsetV1 blockTableLength
        else return (hashTableOffsetV1, blockTableOffsetV1, 0, 0)

    archiveDataLengthV3 <- if mpqVersion >= 2 && headerSize >= 0x44
        then parseV3
        else return $ if highBlockTableOffset > hashTableOffsetV2
            then if highBlockTableOffset > blockTableOffsetV2
                then highBlockTableOffset + 4 * blockTableLength
                else blockTableOffsetV2 + 16 * blockTableLength
            else hashTableOffsetV2 + 16 * hashTableLength

    (hashTableCompressedSizeV4, blockTableCompressedSizeV4, highBlockTableCompressedSize) <- if mpqVersion >= 3
        then parseV4
        else return (
            hashTableCompressedSize,
            blockTableCompressedSize,
            if highBlockTableOffset > 0
                then 4 * blockTableLength
                else 0)

    let hashTableSize = 16 * hashTableLength
    let blockTableSize = 16 * blockTableLength
    let highBlockTableSize = if highBlockTableOffset /= 0
        then 4 * blockTableLength
        else 0

    -- TODO: check for strong signature presence

    return $ MpqHeader headerSize archiveDataLengthV3 blockSize hashTableLength hashTableOffsetV2 hashTableCompressedSizeV4 blockTableLength blockTableOffsetV2 blockTableCompressedSize highBlockTableOffset highBlockTableCompressedSize

parseV2 :: Word -> Word -> Word -> Get (Word, Word, Word, Word)
parseV2 hashTableOffsetV1 blockTableOffsetV1 blockTableLength = do
    highBlockTableOffset <- getWord64le >>= return . fromIntegral
    let highBlockTableCompressedSize = if highBlockTableOffset /= 0
        then 4 * blockTableLength
        else 0
    hashTableOffsetHigh <- getWord16le >>= return . fromIntegral
    blockTableOffsetHigh <- getWord16le >>= return . fromIntegral
    let hashTableOffsetV2 = hashTableOffsetV1 `xor` (hashTableOffsetHigh `shift` 32)
    let blockTableOffsetV2 = blockTableOffsetV1 `xor` (blockTableOffsetHigh `shift` 32)

    return (
        hashTableOffsetV2,
        blockTableOffsetV2,
        highBlockTableOffset,
        highBlockTableCompressedSize)

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
    return (
        hashTableCompressedSize,
        blockTableCompressedSize,
        highBlockTableCompressedSize)

readHashTable :: MpqHeader -> Get [ MpqHashEntry ]
readHashTable header = do
    skip $ archiveDataOffset + (fromIntegral $ hashTableOffset header)
    words <- readEncryptedUInt32Table (hashTableLength header) (hashTableCompressedSize header) (hash "(hash table)" 0x300)
    return $ buildEntries words
        where buildEntries [] = []
              buildEntries (hashA : hashB : locale : block : rest) = MpqHashEntry hashA hashB (fromIntegral locale) (fromIntegral block) : (buildEntries rest)

readBlockTable :: MpqHeader -> Get [ MpqBlockEntry ]
readBlockTable header = do
    skip $ archiveDataOffset + (fromIntegral $ blockTableOffset header)
    words <- readEncryptedUInt32Table (blockTableLength header) (blockTableCompressedSize header) (hash "(block table)" 0x300)
    when (highBlockTableOffset header /= 0 && highBlockTableCompressedSize header /= 0) $ fail "high table not supported"
    -- TODO: file index and count
    return $ buildEntries words
        where buildEntries [] = []
              buildEntries (offset : compressedSize : uncompressedSize : flags : rest) = MpqBlockEntry offset compressedSize uncompressedSize flags : (buildEntries rest)

readEncryptedUInt32Table :: Word -> Word -> Word -> Get [ Word ]
readEncryptedUInt32Table tableLength dataLength hashValue = do
    let uintCount = tableLength `shiftL` 2
    when (uintCount * 4 > dataLength) $ fail "compressed not supported"
    bytes <- getByteString $ fromIntegral dataLength
    -- TODO: bit order depends only on the machine where encryption is performed
    return $ decrypt hashValue . cs2ws $ B.unpack bytes
        where cs2ws [] = []
              cs2ws (a:b:c:d:rest) = (fromIntegral a .|. (fromIntegral b `shift` 8) .|. (fromIntegral c `shift` 16) .|. (fromIntegral d `shift` 24)) : (cs2ws rest)

findBlock :: [ MpqHashEntry ] -> String -> Int
findBlock hashEntries filename = find new_table
    where start = fromIntegral (hash filename 0) `mod` (length hashEntries)
          (a, b) = splitAt start hashEntries
          new_table = b ++ a
          -- TODO: locale
          find [] = -1
          find (x:xs) = if isEntryInvalid x
              then -1
              else if hashMatch x filename
                  then (block x)
                  else find xs

findMultiBlock :: [ MpqHashEntry ] -> String -> [ Int ]
findMultiBlock hashEntries filename = findMulti new_table
    where (a, b) = splitAt (fromIntegral (hash filename 0) `mod` (length hashEntries)) hashEntries
          new_table = b ++ a
          -- TODO: locale
          findMulti [] = []
          findMulti (x:xs) = if isEntryInvalid x
              then []
              else if hashMatch x filename
                  then (block x):(findMulti xs)
                  else findMulti xs

parseFile :: String -> IO ()
parseFile path = do
    contents <- L.readFile path
    let header = runGet parseHeader contents
    print header
    let hashEntries = runGet (readHashTable header) contents
    print hashEntries
    let blockEntries = runGet (readBlockTable header) contents
    print blockEntries
    print $ findBlock hashEntries "(listfile)"
    print $ findMultiBlock hashEntries "(listfile)"

main = parseFile "test.mpq"
