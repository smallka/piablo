module Crypto
    (
        hash,
        decrypt
    ) where

import Data.Word
import Data.List
import Data.Bits
import Data.Char

build1D :: Word -> Word -> Word -> [ Word ]
build1D 0 q r = []
build1D len q r = seed : (build1D (len - 1) new_q new_r)
    where mid_r = (r * 125 + 3) `mod` 0x2AAAAB
          mid_seed = (mid_r .&. 0xFFFF) `shiftL` 16
          (new_q, new_r) = divMod (mid_r * 125 + 3) 0x2AAAAB
          seed = mid_seed .|. (new_r .&. 0xFFFF)

split2D :: [ Word ] -> [ [ Word ] ]
split2D [] = []
split2D all = first_row : (split2D left)
    where (first_row, left) = splitAt 5 all

hashTable :: [ Word ]
hashTable = concat $ transpose $ split2D $ build1D 0x500 0x100001 0x100001

hash :: String -> Int -> Word
hash text hashOffset = fst $ foldl calc_hash (0x7FED7FED, 0xEEEEEEEE) (map ord text)
    where calc_hash (value, seed) c =
            let c' = if c >= 128 then (ord '?') else c
                b = if c' > 0x60 && c' < 0x7B then (c' - 0x20) else c'
                new_value = hashTable !! (hashOffset + b) `xor` (value + seed)
                new_seed = seed + new_value + (seed `shiftL` 5) + (fromIntegral b) + 3
            in (new_value, new_seed)

decrypt :: Word -> [ Word ] -> [ Word ]
decrypt hashValue words = fst $ foldl decrypt_hash ([], (hashValue, 0xEEEEEEEE)) words
    where decrypt_hash (plains, (value, temp)) word =
            let temp' = temp + (hashTable !! (0x400 + (fromIntegral value .&. 0xFF)))
                buffer = word `xor` (temp' + value)
                new_temp = temp' + buffer + (temp' `shiftL` 5) + 3
                new_value = (value `shiftR` 11) .|. (0x11111111 + ((value `xor` 0x7FF) `shiftL` 21))
            in (plains ++ [buffer], (new_value, new_temp))

