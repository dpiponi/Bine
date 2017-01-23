{-# LANGUAGE FlexibleContexts #-}

module OSWORD where

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

{-# INLINABLE osword #-}
osword :: (MonadInput m, MonadState State6502 m, Emu6502 m) => m ()
osword = do
    a <- getA
    x <- getX
    y <- getY
    --liftIO $ putStrLn $ "OSWORD " ++ show (a, x, y)

    case a of
        -- Read line.
        0x00 -> do
            lo <- getX
            hi <- getY
            let addr = make16 lo hi
            sAddrLo <- readMemory addr
            sAddrHi <- readMemory (addr+1)
            let sAddr = make16 sAddrLo sAddrHi
            --line <- liftIO $ getLine
            --Just line <- M $ lift $ getInputLine ""
            Just line <- inputLine ""
            let n = length line
            forM_ [0..n-1] $ \i -> do
                writeMemory (sAddr+i16 i) (BS.c2w (line!!i))
            writeMemory (sAddr+i16 n) 13
            putC False
            putY $ i8 n+1

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
