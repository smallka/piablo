import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString as B
import Data.Binary.Get
import Data.Word (Word)
import Data.Bits
import Control.Monad (when)
import Codec.Compression.Zlib (decompress)
import Text.Groom (groom)

import Crypto (hash, decrypt)

data MpqHeader = MpqHeader {
    headerSize :: Word,
    archiveDataLength :: Word,
    version :: Word,
    blockSize :: Word,
    hashTableOffset :: Word,
    blockTableOffset :: Word,
    hashTableLength :: Word,
    blockTableLength :: Word,
    -- Version 2 (Burning Crusade)
    highBlockTableOffset :: Word,
    hashTableOffsetHigh :: Word,
    blockTableOffsetHigh :: Word,
    -- Version 3 (Cataclysm First)
    archiveDataLength64 :: Word,
    ebtOffset :: Word,
    ehtOffset :: Word,
    -- Version 4 (Cataclysm Second)
    hashTableCompressedSize :: Word,
    blockTableCompressedSize :: Word,
    highBlockTableCompressedSize :: Word,
    ebtCompressedSize :: Word,
    ehtCompressedSize :: Word,
    rawChunkSize :: Word
} deriving (Show)

-- TODO: handle MPQ Data in middle
archiveDataOffset = 0

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
    version <- gw16
    when (version /= 3) $ fail "only support version 4 (Cataclysm Second)"

    blockSize <- gw16 >>= return . (shift 0x200) . fromIntegral
    hashTableOffset <- gw32
    blockTableOffset <- gw32
    hashTableLength <- gw32
    blockTableLength <- gw32

    highBlockTableOffset <- gw64
    hashTableOffsetHigh <- gw16
    blockTableOffsetHigh <- gw16

    archiveDataLength64 <- gw64
    ebtOffset <- gw64
    ehtOffset <- gw64

    hashTableCompressedSize <- gw64
    blockTableCompressedSize <- gw64
    highBlockTableCompressedSize <- gw64
    ebtCompressedSize <- gw64
    ehtCompressedSize <- gw64
    rawChunkSize <- gw32

    skip $ 6 * 16 -- hashes
    return $ MpqHeader headerSize archiveDataLength version blockSize hashTableOffset blockTableOffset hashTableLength blockTableLength highBlockTableOffset hashTableOffsetHigh blockTableOffsetHigh archiveDataLength64 ebtOffset ehtOffset hashTableCompressedSize blockTableCompressedSize highBlockTableCompressedSize ebtCompressedSize ehtCompressedSize rawChunkSize

checkHeader :: MpqHeader -> IO ()
checkHeader header = do
    when (hashTableLength header * 16 /= (blockTableOffset header) - (hashTableOffset header)) $ fail "compressed hash table not supported"
    when (blockTableLength header * 16 /= (archiveDataLength header) - (blockTableOffset header)) $ fail "compressed block table not supported"
    when (highBlockTableOffset header /= 0) $ fail "Hi-Block Table not supported"
    when (hashTableOffsetHigh header /= 0) $ fail "hash Table High bit not supported"
    when (blockTableOffsetHigh header /= 0) $ fail "block Table High bit not supported"
    when (archiveDataLength64 header /= archiveDataLength header) $ fail "64-bit archive size not supported"
    when (hashTableCompressedSize header /= (hashTableLength header * 16)) $ fail "compressed hash table not supported"
    when (blockTableCompressedSize header /= (blockTableLength header * 16)) $ fail "compressed block table not supported"
    when (highBlockTableCompressedSize header /= 0) $ fail "Hi-Block Table not supported"
    -- TODO: check for strong signature presence

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

readHashTable :: MpqHeader -> Get [ MpqHashEntry ]
readHashTable header = do
    skip $ archiveDataOffset + (fromIntegral $ hashTableOffset header)
    words <- readEncryptedUInt32Table (hashTableLength header) (hash "(hash table)" 0x300)
    return $ buildEntries words
        where buildEntries [] = []
              buildEntries (hashA : hashB : locale : block : rest) = MpqHashEntry hashA hashB (fromIntegral locale) (fromIntegral block) : (buildEntries rest)

data MpqBlockEntry = MpqBlockEntry {
    offset :: Word,
    compressedSize :: Word,
    uncompressedSize :: Word,
    flags :: Word
} deriving (Show)

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
    let dataLength = tableLength * 16
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

isSingleBlock :: MpqBlockEntry -> Bool
isSingleBlock entry = (flags entry .&. 0x1000000) /= 0

isDclCompressed :: MpqBlockEntry -> Bool
isDclCompressed entry = (flags entry .&. 0x100) /= 0

isMultiCompressed :: MpqBlockEntry -> Bool
isMultiCompressed entry = (flags entry .&. 0x200) /= 0

isEncrypted :: MpqBlockEntry -> Bool
isEncrypted entry = (flags entry .&. 0x10000) /= 0

isPatch :: MpqBlockEntry -> Bool
isPatch entry = (flags entry .&. 0x100000) /= 0

readBlockOffsets :: MpqHeader -> MpqBlockEntry -> Get [ Word ]
readBlockOffsets header entry = do
    when (isPatch entry) $ fail "patch block not supported"
    when (isEncrypted entry) $ fail "entrypted block not supported"
    when (isDclCompressed entry) $ fail "DCL compressed block not supported"
    when (not $ isMultiCompressed entry) $ fail "uncompressed block not supported"
    let blockCount = (uncompressedSize entry + (blockSize header) - 1) `div` (blockSize header) + 1
    if isSingleBlock entry
        then return [0,  (compressedSize entry)]
        else do
            skip $ fromIntegral $ offset entry
            readBlockOffsets' blockCount []
        where readBlockOffsets' 0 offsets = return offsets
              readBlockOffsets' count offsets = do
                  next <- gw32
                  readBlockOffsets' (count - 1) (offsets ++ [ next ])

readIt :: [ Word ] -> L.ByteString -> L.ByteString
readIt (cur:next:rest) blocks = L.append plaintext $ readIt (next:rest) blocks
    where compressed = L.drop (fromIntegral cur) $ L.take (fromIntegral next) blocks
          plaintext = decompress $ L.drop 1 compressed
readIt _ blocks = L.empty

readMpq :: String -> IO ()
readMpq path = do
    contents <- L.readFile path
    let header = runGet parseHeader contents
    putStrLn . groom $ header
    checkHeader header

    let hashEntries = runGet (readHashTable header) contents
    let blockEntries = runGet (readBlockTable header) contents

    let listfile = blockEntries !! (findBlock hashEntries "(listfile)")
    let offsets = runGet (readBlockOffsets header listfile) contents
    let blocks = L.drop (fromIntegral $ offset listfile) contents
    let filenames = lines $ L8.unpack $ readIt offsets blocks
    -- putStrLn . groom $ filenames

    print $ findMultiBlock hashEntries "(listfile)"
    return ()

main = readMpq "../mpq/enUS_Text.mpq"