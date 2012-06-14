module Crypto
    (
        hash,
        decrypt
    ) where

import Data.Word (Word)
import Data.List (transpose)
import Data.Char (ord)
import Data.Bits

build1D :: Word -> Word -> Word -> [ Word ]
build1D 0 _ _ = []
build1D len _ r = seed : (build1D (len - 1) new_q new_r)
    where mid_r = (r * 125 + 3) `mod` 0x2AAAAB
          mid_seed = (mid_r .&. 0xFFFF) `shiftL` 16
          (new_q, new_r) = divMod (mid_r * 125 + 3) 0x2AAAAB
          seed = mid_seed .|. (new_r .&. 0xFFFF)

split2D :: [ Word ] -> [ [ Word ] ]
split2D [] = []
split2D wordList = first_row : (split2D left)
    where (first_row, left) = splitAt 5 wordList

hashTable :: [ Word ]
hashTable = concat . transpose . split2D $ build1D 0x500 0x100001 0x100001

hash :: String -> Int -> Word
hash text hashOffset = fst $ foldl calc_hash (0x7FED7FED, 0xEEEEEEEE) bytes
    where bytes = map ord text
          calc_hash (value, seed) c = (new_value, new_seed)
              where c' = if c >= 128 then (ord '?') else c
                    b = if c' > 0x60 && c' < 0x7B then (c' - 0x20) else c'
                    new_value = hashTable !! (hashOffset + b) `xor` (value + seed)
                    new_seed = seed + new_value + (seed `shiftL` 5) + (fromIntegral b) + 3

decrypt :: Word -> [ Word ] -> [ Word ]
decrypt hashValue encrypted = plains
    where (plains, _, _) = foldl decrypt_hash ([], hashValue, 0xEEEEEEEE) encrypted
          decrypt_hash (decrypted, value, temp) word = (decrypted ++ [buffer], new_value, new_temp)
              where temp' = temp + (hashTable !! (0x400 + (fromIntegral value .&. 0xFF)))
                    buffer = word `xor` (temp' + value)
                    new_temp = temp' + buffer + (temp' `shiftL` 5) + 3
                    new_value = (value `shiftR` 11) .|. (0x11111111 + ((value `xor` 0x7FF) `shiftL` 21))
