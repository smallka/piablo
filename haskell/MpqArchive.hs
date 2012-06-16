import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString as B
import Data.Binary.Get
import Data.Word (Word, Word16, Word32, Word64)
import Data.Bits
import Control.Applicative ((<$>), (<*>))
import Control.Monad (when)
import Codec.Compression.Zlib (decompress)
import Text.Groom (groom)

import MpqBinary
import Crypto (hash, decrypt)

data MpqHeader = MpqHeader {
    signature :: Word32,
    headerSize :: Word32,
    archiveDataLength :: Word32,
    version :: Word16,
    blockSizeExp :: Word16,
    hashTableOffset :: Word32,
    blockTableOffset :: Word32,
    hashTableLength :: Word32,
    blockTableLength :: Word32,
    -- Version 2 (Burning Crusade)
    highBlockTableOffset :: Word64,
    hashTableOffsetHigh :: Word16,
    blockTableOffsetHigh :: Word16,
    -- Version 3 (Cataclysm First)
    archiveDataLength64 :: Word64,
    ebtOffset :: Word64,
    ehtOffset :: Word64,
    -- Version 4 (Cataclysm Second)
    hashTableCompressedSize :: Word64,
    blockTableCompressedSize :: Word64,
    highBlockTableCompressedSize :: Word64,
    ebtCompressedSize :: Word64,
    ehtCompressedSize :: Word64
    -- NOTE: AutoGet require context stack to go deep, and GHC default to 20
    -- which is too small for the rest fields to get in
    -- rawChunkSize :: Word32
    -- hashes 6 * 16
} deriving (Show)

blockSize :: MpqHeader -> Word
blockSize header = 0x200 `shift` (fromIntegral $ blockSizeExp header)

-- TODO: handle MPQ Data in middle
archiveDataOffset = 0

gw16 = getWord16le >>= return . fromIntegral
gw32 = getWord32le >>= return . fromIntegral
gw64 = getWord64le >>= return . fromIntegral

class AutoGet a where
    autoGet :: a -> Get MpqHeader

instance AutoGet MpqHeader where
    autoGet m = return m

instance (MpqBinary x, AutoGet y) => AutoGet (x -> y) where
    autoGet f = do x <- getMB
                   autoGet (f x)

-- when (signature == 0x1B51504D) $ fail "user data not supported"
-- when (signature /= 0x1A51504D) $ fail "invalid archive signature
-- when (version /= 3) $ fail "only support version 4 (Cataclysm Second

parseHeader :: Get MpqHeader
parseHeader = autoGet MpqHeader
parseHeader' = MpqHeader <$> getMB <*> getMB <*> getMB <*> getMB <*> getMB <*> getMB <*> getMB <*> getMB <*> getMB <*> getMB <*> getMB <*> getMB <*> getMB <*> getMB <*> getMB <*> getMB <*> getMB <*> getMB <*> getMB <*> getMB

checkHeader :: MpqHeader -> Maybe String
checkHeader header = case dropWhile fst conds of
        (x:xs) -> Just $ snd x
        otherwise -> Nothing
    where conds = [
            (True, "hello"),
            (hashTableLength header * 16 == (blockTableOffset header) - (hashTableOffset header), "compressed hash table not supported"),
            (blockTableLength header * 16 == (archiveDataLength header) - (blockTableOffset header), "compressed block table not supported"),
            (highBlockTableOffset header == 0, "Hi-Block Table not supported"),
            (hashTableOffsetHigh header == 0, "hash Table High bit not supported"),
            (blockTableOffsetHigh header == 0, "block Table High bit not supported"),
            (archiveDataLength64 header == fromIntegral (archiveDataLength header), "64-bit archive size not supported"),
            (hashTableCompressedSize header == fromIntegral (hashTableLength header * 16), "compressed hash table not supported"),
            (blockTableCompressedSize header == fromIntegral (blockTableLength header * 16), "compressed block table not supported"),
            (highBlockTableCompressedSize header == 0, "Hi-Block Table not supported")
            -- TODO: check for strong signature presence
            ]

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
readEncryptedUInt32Table :: Word32 -> Word -> Get [ Word ]
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

    case checkHeader header of
        Just reason -> print reason
        otherwise -> return ()

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
