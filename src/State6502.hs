{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module State6502 where

import Data.Word
import Deque
import Data.Array
import Data.Time.Clock
import Control.Lens
import Control.Monad.State
import Data.Bits.Lens
import System.Console.Haskeline
import Data.Array.IO
import qualified Data.IntMap as M
import System.IO
import Data.ByteString as B
import FileSystems

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

data KeyInput = KeyInput {
                    buffer :: Deque Word8,
                    keydefs :: Array Int [Word8]
                }

data VDUOutput = VDUOutput {
                    vbuffer :: [Word8],
                    requiredChars :: Int
                 }

data State6502 = S {
    _mem :: IOUArray Int Word8,
    _clock :: !Int,
    _regs :: !Registers,
    _debug :: !Bool,

    _currentDirectory :: Char,
    _sysclock :: !UTCTime,
    _handles :: M.IntMap VHandle,
    _keyQueue :: KeyInput,
    _vduQueue :: VDUOutput,
    _logFile :: Maybe Handle
}

makeLenses ''State6502

