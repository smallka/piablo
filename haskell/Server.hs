{-# LANGUAGE OverloadedStrings #-}

import Data.Conduit
import Data.Conduit.Network
import Data.Conduit.Binary as CB
import Data.Bits
import Data.ByteString as S
import Data.ByteString.Char8 as S8
import Data.Binary.Get (runGet, getWord32be)
import System.IO (stdout)

handlePacket :: Resource m => Conduit S.ByteString m S.ByteString
handlePacket = sequenceSink () $ \() -> do
    bytes <- CB.take 4
    let size = fromIntegral $ runGet getWord32be bytes
    content <- CB.take (size - 4)
    return $ Emit () [S8.pack $ show size]

main :: IO ()
main = runTCPServer (ServerSettings 1999 $ Just "127.0.0.1") $
    \src sink -> src $= handlePacket $$ CB.sinkHandle stdout
