{-# LANGUAGE FlexibleContexts #-}

module OSWORD where

import Text.Printf
import Control.Monad.State
import System.IO
import Utils
import MonadInput
import Data.Time.Clock
import Control.Lens
import Data.List
import KeyInput
import State6502
import Monad6502
import Utils
import Core
import TraceLog
import qualified Data.IntMap as M
import qualified Data.ByteString.Internal as BS (c2w, w2c)
import Data.Word
import Numeric

readLine :: (MonadInput m, MonadState State6502 m, Emu6502 m) => Word8 -> Word8 -> Word8 -> m ()
readLine a x y = do
    lo <- getX
    hi <- getY
    let addr = make16 lo hi
    sAddrLo <- readMemory addr
    sAddrHi <- readMemory (addr+1)
    maxLength <- readMemory (addr+2)
    minChar <- readMemory (addr+3)
    maxChar <- readMemory (addr+4)
    let sAddr = make16 sAddrLo sAddrHi
    --line <- liftIO $ getLine
    --Just line <- M $ lift $ getInputLine ""
    queue <- use keyQueue
    -- Write fn to just get chars up to 0xd XXX
    allkeys <- allKeys
    let (prefix, rest) = break (== 13) allkeys

    tracelog $ printf " Reading line to %04x, max length=%d, min=%d, max=%d..." sAddr maxLength minChar maxChar

    liftIO $ hSetEcho stdin True

    line <- if null rest
        then do
            -- XXX Need to deal with case where
            -- there is already a carriage return in prefix
            -- Implement allKeysUpToCR or something XXX
            Just line <- inputLineWithInitial "" (map BS.w2c prefix, "")
            return (map BS.c2w line)
        else do
            putKeys (tail rest)
            liftIO $ putStrLn (map BS.w2c prefix)
            return prefix
    let n = length line
    -- XXX redo with zip
    forM_ [0..n-1] $ \i -> do
        writeMemory (sAddr+i16 i) (line!!i)
    writeMemory (sAddr+i16 n) 0xd
    putC False
    -- Hard to find definitive documentation on
    -- whether this is n or n+1
    putY $ i8 n
    --putY $ i8 n+1
    liftIO $ hSetEcho stdin False

    tracelog $ printf " ...read %s" (show (map BS.w2c line))

{-# INLINABLE osword #-}
osword :: (MonadInput m, MonadState State6502 m, Emu6502 m) => m ()
osword = do
    a <- getA
    x <- getX
    y <- getY

    tracelog $ printf "OSWORD A=%02x X=%02x Y=%02x" a x y

    case a of
        -- Read line.
        0x00 -> readLine a x y
        
        -- Read system clock
        0x01 -> do
            let addr = make16 x y
            t0 <- use sysclock
            t1 <- liftIO $ getCurrentTime
            let diff = t1 `diffUTCTime` t0
            let a = floor (100*diff)
            putWord32 addr (fromIntegral a)

        -- Peek memory
        0x05 -> do
            let addr = make16 x y
            peekAddr <- word32At addr
            byte <- readMemory (i16 peekAddr)
            putC False
            putA byte

        -- Envelope
        0x08 -> do
            putC False

        _ -> liftIO $ putStrLn $ "Unknown OSWORD call: " ++ show a ++ "," ++ show x ++ "," ++ show y
