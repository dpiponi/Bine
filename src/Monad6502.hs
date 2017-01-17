{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Monad6502 where

import Data.Word
import Control.Lens
import Control.Monad.State
import Data.Bits.Lens
import System.Console.Haskeline
import Data.Array.IO
import qualified Data.IntMap as M
import Core
import System.IO
import State6502
import Utils
import Data.Bits
import qualified Data.ByteString.Internal as BS (c2w, w2c)

newtype Monad6502 a = M { unM :: StateT State6502 (InputT IO) a }
    deriving (Functor, Applicative, Monad, MonadState State6502, MonadIO)

{-# INLINABLE stringAt #-}
stringAt :: Emu6502 m => Word16 -> m String
stringAt addr = do
    let loop cmd i = do
                    byte <- readMemory (addr+i16 i)
                    if byte /= 0x0d
                        then loop (cmd ++ [BS.w2c byte]) (i+1)
                        else return cmd
    loop "" 0

{-# INLINE word16At #-}
word16At :: Emu6502 m => Word16 -> m Word16
word16At addr = do
    lo <- readMemory addr
    hi <- readMemory (addr+1)
    return $ make16 lo hi

{-# INLINE word32At #-}
word32At :: Emu6502 m => Word16 -> m Word32
word32At addr = do
    b0 <- readMemory addr
    b1 <- readMemory (addr+1)
    b2 <- readMemory (addr+2)
    b3 <- readMemory (addr+3)
    return $ make32 b0 b1 b2 b3

{-# INLINE putWord32 #-}
putWord32 :: Emu6502 m => Word16 -> Word32 -> m ()
putWord32 addr w = do
    writeMemory addr (i8 $ w)
    writeMemory (addr+1) (i8 (w `shift` (-8)))
    writeMemory (addr+2) (i8 (w `shift` (-16)))
    writeMemory (addr+3) (i8 (w `shift` (-24)))
