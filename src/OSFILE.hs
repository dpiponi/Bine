{-# LANGUAGE FlexibleContexts #-}

module OSFILE where

import FileSystems
import Control.Monad.State
import System.IO
import Utils
import Control.Lens
import State6502
import Monad6502
import Utils
import Core
import qualified Data.IntMap as M
import qualified Data.ByteString.Internal as BS (c2w, w2c)
import Data.Word
import Numeric

{-# INLINABLE osfile #-}
osfile :: (MonadState State6502 m, Emu6502 m) => m ()
osfile = do
    a <- getA
    x <- getX
    y <- getY

    let blockAddr = make16 x y
    stringAddr <- word16At blockAddr
    filename <- stringAt stringAddr
    
    case a of
        -- Save block
        0 -> do
            startData32 <- word32At (blockAddr+0xa)
            endData32 <- word32At (blockAddr+0xe)
            let start = i16 startData32
            let end = i16 endData32
            liftIO $ putStrLn $ "Saving " ++ showHex start "" ++ ":" ++ showHex end "" ++ " to " ++ filename
            h <- liftIO $ openFile filename WriteMode
            forM_ [start..end-1] $ \i -> do
                x <- readMemory i
                liftIO $ hPutChar h (BS.w2c x)
            liftIO $ hClose h

        0xff -> do
            loadAddr32 <- word32At (blockAddr+0x2)
            let start = i16 loadAddr32
            h <- liftIO $ openFile filename ReadMode
            bytes <- liftIO $ hGetContents h
            let len = length bytes
            let end = start+fromIntegral len
            liftIO $ putStrLn $ "Reading " ++ showHex start "" ++ ":" ++ showHex end "" ++ " from " ++ filename
            forM_ (zip [start..end-1] bytes) $ \(i, d) -> writeMemory i (BS.c2w d)
            liftIO $ hClose h

        _ -> error $ "Unknown OSFILE call " ++ show a ++ "," ++ show x ++ "," ++ show y
