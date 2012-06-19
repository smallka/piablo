import qualified Data.ByteString.Lazy as LS
import qualified Data.ByteString.Lazy.Char8 as LS8
import Data.Binary.Get
import Data.Word (Word16, Word32, Word64)
import Data.Bits
import Control.Applicative ((<$>), (<*>))
import Control.Monad (when, replicateM)
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

blockSize :: MpqHeader -> Word32
blockSize header = 0x200 `shift` (fromIntegral $ blockSizeExp header)

-- TODO: handle MPQ Data in middle
archiveDataOffset = 0

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
    hashA :: Word32,
    hashB :: Word32,
    locale :: Word32,
    block :: Word32
} deriving (Show)

isEntryInvalid :: MpqHashEntry -> Bool
isEntryInvalid entry = block entry == 0xFFFFFFFF || hashA entry == 0xFFFFFFFF || hashB entry == 0xFFFFFFFF

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
    offset :: Word32,
    compressedSize :: Word32,
    uncompressedSize :: Word32,
    flags :: Word32
} deriving (Show)

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

readBlockTable :: MpqHeader -> Get [ MpqBlockEntry ]
readBlockTable header = do
    skip $ archiveDataOffset + (fromIntegral $ blockTableOffset header)
    words <- readEncryptedUInt32Table (blockTableLength header) (hash "(block table)" 0x300)
    -- TODO: file index and count
    return $ buildEntries words
        where buildEntries [] = []
              buildEntries (offset : compressedSize : uncompressedSize : flags : rest) = MpqBlockEntry offset compressedSize uncompressedSize flags : (buildEntries rest)

readEncryptedUInt32Table :: Word32 -> Word32 -> Get [ Word32 ]
readEncryptedUInt32Table tableLength hashValue = do
    encrypted <- replicateM (fromIntegral $ tableLength * 4) getMB
    return $ decrypt hashValue encrypted

findBlock :: [ MpqHashEntry ] -> String -> Maybe Word32
findBlock hashEntries filename = find new_table
    where start = fromIntegral (hash filename 0) `mod` (length hashEntries)
          (a, b) = splitAt start hashEntries
          new_table = b ++ a
          -- TODO: locale
          find [] = Nothing
          find (x:xs) = if isEntryInvalid x
              then Nothing
              else if hashMatch x filename
                  then Just (block x)
                  else find xs

findMultiBlock :: [ MpqHashEntry ] -> String -> [ Word32 ]
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

findFile :: MpqHeader -> [ MpqHashEntry ] -> [ MpqBlockEntry ] -> LS.ByteString -> String -> Maybe LS.ByteString
findFile header hashEntries blockEntries contents filename = do
    blockIndex <- findBlock hashEntries filename
    let entry = blockEntries !! (fromIntegral $ blockIndex)
    let offsets = runGet (readBlockOffsets header entry) contents
    let blocks = LS.drop (fromIntegral $ offset entry) contents
    return $ readIt offsets blocks

readBlockOffsets :: MpqHeader -> MpqBlockEntry -> Get [ Word32 ]
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
            replicateM (fromIntegral blockCount) getMB

readIt :: [ Word32 ] -> LS.ByteString -> LS.ByteString
readIt (cur:next:rest) blocks = LS.append plaintext $ readIt (next:rest) blocks
    where compressed = LS.drop (fromIntegral cur) $ LS.take (fromIntegral next) blocks
          plaintext = decompress $ LS.drop 1 compressed
readIt _ blocks = LS.empty

parseMpq :: String -> IO ()
parseMpq path = do
    contents <- LS.readFile path
    let header = runGet parseHeader contents
    putStrLn . groom $ header

    case checkHeader header of
        Just reason -> print reason
        otherwise -> return ()

    let hashEntries = runGet (readHashTable header) contents
    let blockEntries = runGet (readBlockTable header) contents

    case findFile header hashEntries blockEntries contents "(listfile)" of
        (Just listfile) -> putStrLn . groom . lines . LS8.unpack $ listfile
        otherwise -> fail "(listfile) not found"

    print $ findMultiBlock hashEntries "(listfile)"
    return ()

main = parseMpq "../mpq/enUS_Text.mpq"
