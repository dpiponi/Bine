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
import qualified Data.IntMap as I
import qualified Data.Map as M
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as BS (c2w, w2c)
import Data.Word
import Numeric

-- http://mdfs.net/Docs/Comp/BBC/API/OSFILE.htm
{-# INLINABLE osfile #-}
osfile :: (MonadState State6502 m, Emu6502 m) => m ()
osfile = do
    a <- getA
    x <- getX
    y <- getY

    let blockAddr = make16 x y
    stringAddr <- word16At blockAddr
    filename <- stringAt stringAddr
    fs <- use fileSystem
    
    case fs of
        Host -> case a of
            -- Save a section of memory as a named file.
            -- The file’s catalogue information is also written.
            0x00 -> do
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

            -- Load the named file and read the named file’s catalogue information.
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

        Bytes load exec cat -> case a of
            -- Save a section of memory as a named file.
            -- The file’s catalogue information is also written.
            0x00 -> error $ "Failed (readonly) OSFILE call " ++ show a ++ "," ++ show x ++ "," ++ show y

            -- Load the named file and read the named file’s catalogue information.
            0xff -> do
                loadAddr32 <- word32At (blockAddr+0x2)
                execAddr32 <- word32At (blockAddr+0x6)
                startData32 <- word32At (blockAddr+0xa)
                addressType <- readMemory (blockAddr+0x6)
                let bytes = cat M.! filename -- XXX unsafe
                let actualAddress = if addressType == 0 then load else loadAddr32
                let start = i16 actualAddress
                let len = B.length bytes
                let end = start+fromIntegral len
                liftIO $ putStrLn $ "Reading " ++ showHex start "" ++ ":" ++ showHex end "" ++ " from " ++ filename
                forM_ (zip [start..end-1] (B.unpack bytes)) $ \(i, d) -> writeMemory i d

            _ -> error $ "Unknown OSFILE call " ++ show a ++ "," ++ show x ++ "," ++ show y
