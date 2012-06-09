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
    mpqVersion :: Word,
    blockSize :: Word,
    hashTableOffset :: Word,
    blockTableOffset :: Word,
    hashTableLength :: Word,
    blockTableLength :: Word,
    v2 :: Maybe MpqHeaderV2
} deriving (Show)

data MpqHeaderV2 = MpqHeaderV2 {
    highBlockTableOffset :: Word,
    hashTableOffsetHigh :: Word,
    blockTableOffsetHigh :: Word,
    v3 :: Maybe MpqHeaderV3
} deriving (Show)

data MpqHeaderV3 = MpqHeaderV3 {
    archiveDataLength64 :: Word,
    ebtOffset :: Word,
    ehtOffset :: Word,
    v4 :: Maybe MpqHeaderV4
} deriving (Show)

data MpqHeaderV4 = MpqHeaderV4 {
    hashTableCompressedSize :: Word,
    blockTableCompressedSize :: Word,
    highBlockTableCompressedSize :: Word,
    ebtCompressedSize :: Word,
    ehtCompressedSize :: Word,
    rawChunkSize :: Word
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

gw16 = getWord16le >>= return . fromIntegral
gw32 = getWord32le >>= return . fromIntegral
gw64 = getWord64le >>= return . fromIntegral

parseHeader :: Get MpqHeader
parseHeader = do
    signature <- gw32
    when (signature == 0x1B51504D) $ fail "user data not supported"
    when (signature /= 0x1A51504D) $ fail "invalid archive signature"

    headerSize <- gw32
    archiveDataLength <- gw32
    mpqVersion <- gw16
    blockSize <- gw16 >>= return . (shift 0x200) . fromIntegral
    hashTableOffset <- gw32
    blockTableOffset <- gw32
    hashTableLength <- gw32
    blockTableLength <- gw32

    v2 <- if mpqVersion >= 1
        then parseV2 mpqVersion >>= return . Just
        else return Nothing

    -- TODO: check for strong signature presence
    return $ MpqHeader headerSize archiveDataLength mpqVersion blockSize hashTableOffset blockTableOffset hashTableLength blockTableLength v2

parseV2 :: Word -> Get MpqHeaderV2
parseV2 mpqVersion = do
    highBlockTableOffset <- gw64
    hashTableOffsetHigh <- gw16
    blockTableOffsetHigh <- gw16

    v3 <- if mpqVersion >= 2
        then parseV3 mpqVersion >>= return . Just
        else return Nothing

    return $ MpqHeaderV2 highBlockTableOffset hashTableOffsetHigh blockTableOffsetHigh v3

parseV3 :: Word -> Get MpqHeaderV3
parseV3 mpqVersion = do
    archiveDataLength64 <- gw64
    ebtOffset <- gw64
    ehtOffset <- gw64

    v4 <- if mpqVersion >= 3
        then parseV4 >>= return . Just
        else return Nothing

    return $ MpqHeaderV3 archiveDataLength64 ebtOffset ehtOffset v4

parseV4 :: Get MpqHeaderV4
parseV4 = do
    hashTableCompressedSize <- gw64
    blockTableCompressedSize <- gw64
    highBlockTableCompressedSize <- gw64
    ebtCompressedSize <- gw64
    ehtCompressedSize <- gw64
    rawChunkSize <- gw32
    skip $ 6 * 16 -- hashes
    return $ MpqHeaderV4 hashTableCompressedSize blockTableCompressedSize highBlockTableCompressedSize ebtCompressedSize ehtCompressedSize rawChunkSize

readHashTable :: MpqHeader -> Get [ MpqHashEntry ]
readHashTable header = do
    skip $ archiveDataOffset + (fromIntegral $ hashTableOffset header)
    words <- readEncryptedUInt32Table (hashTableLength header) (hash "(hash table)" 0x300)
    return $ buildEntries words
        where buildEntries [] = []
              buildEntries (hashA : hashB : locale : block : rest) = MpqHashEntry hashA hashB (fromIntegral locale) (fromIntegral block) : (buildEntries rest)

readBlockTable :: MpqHeader -> Get [ MpqBlockEntry ]
readBlockTable header = do
    skip $ archiveDataOffset + (fromIntegral $ blockTableOffset header)
    words <- readEncryptedUInt32Table (blockTableLength header) (hash "(block table)" 0x300)
    -- TODO: file index and count
    return $ buildEntries words
        where buildEntries [] = []
              buildEntries (offset : compressedSize : uncompressedSize : flags : rest) = MpqBlockEntry offset compressedSize uncompressedSize flags : (buildEntries rest)

-- TODO: make it pure
readEncryptedUInt32Table :: Word -> Word -> Get [ Word ]
readEncryptedUInt32Table tableLength hashValue = do
    let unitCount = tableLength `shiftL` 2
    let dataLength = unitCount * 4
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
    let blockEntries = runGet (readBlockTable header) contents
    print $ findBlock hashEntries "(listfile)"
    print $ findMultiBlock hashEntries "(listfile)"

main = parseFile "test.mpq"
