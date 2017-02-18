{-# LANGUAGE FlexibleContexts #-}

module OSBYTE where

import Control.Monad.State
import System.IO
import Utils
import MonadInput
import KeyInput
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
import TraceLog
import Text.Printf

inkey :: (MonadState State6502 m, Emu6502 m) => Word8 -> Word8 -> m ()
inkey x y = do
    tracelog " Read key with time limit..."
    if y < 0xff
        then do
            result <- liftIO $ hWaitForInput stdin (10*fromIntegral (make16 x y))
            if result
                then do
                    k <- liftIO getChar
                    putX (BS.c2w k)
                    putY 0
                    putC False
                    tracelog $ printf " ...got key %d" k
                else do
                    putY 0xff
                    putC True
                    tracelog " ...timed out"
        else do
            tracelog $ printf " (Key scan for %02x)" x
            putX 0
            putY 0

{-# INLINABLE osbyte #-}
osbyte :: (MonadState State6502 m, Emu6502 m) => Word8 -> Word8 -> Word8 -> m ()
osbyte a x y = do
    tracelog $ printf "OSBYTE A=%02x X=%02x Y=%02x" a x y
    --liftIO $ putStrLn $ "OSBYTE " ++ show (a, x, y)
    case a of
        -- Enable/disable cursor editing
        4 -> do
            tracelog " (Enable/disable cursor editing)"
        -- Wait for vertical sync
        19 -> do
            tracelog " (Wait for vertical sync)"
            return ()
        -- Flush specific buffer
        21 -> if x == 0
                then do
                    tracelog " Flush keyboard buffer"
                    removeKeys
                else tracelog $ printf " (Flush buffer %d)" x
        -- Clear ESCAPE condition
        124 -> do
            tracelog " (Clear ESCAPE condition)"
        126 -> do
            tracelog " Acknowledge ESCAPE condition"
            esc <- readMemory 0xff
            writeMemory 0xff 0x00
            putX esc
        -- Read key with time limit
        129 -> inkey x y
        -- Read machine high order address
        130 -> do
            tracelog " Read machine high order address"
            putX 0xff
            putY 0xff
        -- Read top of operating system RAM address (OSHWM)
        131 -> do
            tracelog " Read top of operating system RAM address (OSHWM)"
            putX 0x00
            putY 0x0e
        -- Read bottom of display RAM address (HIMEM)
        132 -> do
            tracelog " Read bottom of display RAM address (HIMEM)"
            putX 0x00
            putY 0x80
        -- Read bottom of display RAM for a specified mode
        133 -> do
            tracelog " Read bottom of display RAM for a specified mode"
            putX 0x00
            putY 0x80
        -- Read character at text cursor position.
        -- Docs say to use 0 if character not recognised.
        -- Lying about mode 7.
        135 -> do
            tracelog " (Read character at text cursor position.)"
            putX 0
            putY 7
        -- Place character into buffer
        138 -> do
            tracelog $ printf " Place character %d into buffer %d" y x
            when (x == 0) $ insertKey y
        -- Select tape filing system (*TAPE equivalent)
        140 -> do
            tracelog $ printf " (TAPE %d)" x
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

                else tracelog $ printf " Unknown OSBYTE call A=%02x X=%02x Y=%02x " a x y
