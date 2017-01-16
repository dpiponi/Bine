module Utils where

import Data.Word
import Data.Bits

removeStars :: String -> String
removeStars ('*' : cs) = removeStars cs
removeStars cs = cs

{-# INLINE i32 #-}
i32 :: Integral a => a -> Word32
i32 = fromIntegral

{-# INLINE make32 #-}
make32 :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
make32 b0 b1 b2 b3 = (i32 b3 `shift` 24)+(i32 b2 `shift` 32)+(i32 b1 `shift` 8)+i32 b0
