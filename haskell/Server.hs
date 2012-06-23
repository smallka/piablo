{-# LANGUAGE OverloadedStrings #-}

import Data.Word
import Data.Conduit
import Data.Conduit.Network
import qualified Data.Conduit.Binary as CB
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.Binary.Get as G
import qualified D3BitGet as BG
import System.IO (stdout)
import Control.Applicative ((<$>), (<*>))

data Message = JoinBNetGame Word64 Word64 Word64 Word64 Word64 Word32 Word64 Word8 Word32 Word32
    | Unknown Int
    deriving (Show)

parseMessage :: BG.BitGet Message
parseMessage = do
    proto <- fromIntegral <$> BG.getWord16be 9
    case proto of
        10 -> JoinBNetGame <$>
            BG.getWord64be 64 <*>
            BG.getWord64be 64 <*>
            BG.getWord64be 64 <*>
            BG.getWord64be 64 <*>
            BG.getWord64be 64 <*>
            BG.getWord32be 32 <*>
            BG.getWord64be 64 <*>
            BG.getWord8 4 <*>
            BG.getWord32be 32 <*>
            BG.getWord32be 32
        otherwise -> return $ Unknown proto

handlePacket :: Resource m => Conduit S.ByteString m S.ByteString
handlePacket = sequenceSink () $ \() -> do
    bytes <- CB.take 4
    let size = fromIntegral $ G.runGet G.getWord32be bytes
    content <- CB.take (size - 4)
    let msg = G.runGet (BG.runBitGet parseMessage) content
    return $ Emit () [S8.pack $ show msg]

main :: IO ()
main = runTCPServer (ServerSettings 1999 $ Just "127.0.0.1") $
    \src sink -> src $= handlePacket $$ CB.sinkHandle stdout
