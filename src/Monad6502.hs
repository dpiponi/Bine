{-# LANGUAGE TemplateHaskell #-}

module Monad6502 where

import Data.Word
import Control.Lens
import Data.Bits.Lens

data Registers = R {
    _pc :: !Word16,
    _p :: !Word8,
    _a :: !Word8,
    _x :: !Word8,
    _y :: !Word8,
    _s :: !Word8
}

makeLenses ''Registers

{-# INLINE flagC #-}
flagC :: Lens' Registers Bool
flagC = p . bitAt 0

{-# INLINE flagZ #-}
flagZ :: Lens' Registers Bool
flagZ = p . bitAt 1

{-# INLINE flagI #-}
flagI :: Lens' Registers Bool
flagI = p . bitAt 2

{-# INLINE flagD #-}
flagD :: Lens' Registers Bool
flagD = p . bitAt 3

{-# INLINE flagB #-}
flagB :: Lens' Registers Bool
flagB = p . bitAt 4

{-# INLINE flagV #-}
flagV :: Lens' Registers Bool
flagV = p . bitAt 6

{-# INLINE flagN #-}
flagN :: Lens' Registers Bool
flagN = p . bitAt 7
