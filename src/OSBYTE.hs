{-# LANGUAGE FlexibleContexts #-}

module OSBYTE where

import Control.Monad.State
import System.IO
import Utils
import MonadInput
import Control.Lens
import State6502
import Monad6502
import Utils
import Core
import qualified Data.IntMap as M
import qualified Data.ByteString.Internal as BS (c2w, w2c)
import Data.Word
import Numeric
import Data.Bits

{-# INLINABLE osbyte #-}
osbyte :: (MonadState State6502 m, Emu6502 m) => Word8 -> Word8 -> Word8 -> m ()
osbyte a x y = case a of
        -- Clear ESCAPE condition
        124 -> do
            liftIO $ putStrLn "Clear ESCAPE condition"
        126 -> do
            liftIO $ putStrLn "Acknowledge ESCAPE condition"
            esc <- readMemory 0xff
            writeMemory 0xff 0x00
            putX esc
        -- Read machine high order address
        130 -> do
            putX 0xff
            putY 0xff
        -- Read top of operating system RAM address (OSHWM)
        131 -> do
            putX 0x00
            putY 0x0e
        -- Read bottom of display RAM address (HIMEM)
        132 -> do
            putX 0x00
            putY 0x80
        -- Read bottom of display RAM for a specified mode
        133 -> do
            putX 0x00
            putY 0x80
        -- Read/write *EXEC file handle.
        198 -> do
            putX 0
            putY 0
            putC False
        -- Read/write length of soft key string.
        216 -> do
            putX 0
            putY 0
            putC False
        -- Read/write status of ESCAPE key (escape action or ASCII code)
        229 -> do
            liftIO $ putStrLn "Read/write status of ESCAPE key"
            putX 0
            putY 0
            putC False
        _ -> do
            if a >= 0xa6 && a <= 0xff
                then do
                    let addr = i16 a-0xa6+0x236
                    old <- readMemory addr
                    next <- readMemory (addr+1)
                    let new = (old .&. x) `xor` y
                    writeMemory addr new
                    putX old
                    putY next

                else error $ "Unknown OSBYTE call " ++ show a ++ "," ++ show x ++ "," ++ show y
