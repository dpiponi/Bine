{-# LANGUAGE FlexibleContexts #-}

module OSFILE where

import FileSystems
import Control.Monad.State
import System.IO
import Utils
import Control.Lens
import Data.Char
import State6502
import Monad6502
import Utils
import MetaData
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
    
    case a of
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
            execAddr32 <- word32At (blockAddr+0x6)
            startData32 <- word32At (blockAddr+0xa)
            addressType <- readMemory (blockAddr+0x6)
            (fileLoad, fileExec) <- liftIO $ getMetaData filename
            -- If caller-specified execution address ends in zero
            -- use user-specified load address
            -- otherwise use load address in file
            let start = if addressType == 0
                then fromIntegral loadAddr32
                else fromIntegral fileLoad
            writeWord32 (blockAddr+0x6) fileExec
            liftIO $ putStrLn $ "Loading (OSFILE 0xff) from file '" ++ filename ++ "'"
            h <- liftIO $ openFile filename ReadMode
            liftIO $ putStrLn $ "hGetContents " ++ filename
            bytes <- liftIO $ B.hGetContents h
            let len = B.length bytes
            let end = start+fromIntegral len
            liftIO $ putStrLn $ "!!Loading " ++ showHex start "" ++ ":" ++ showHex end "" ++ " from " ++ filename
            forM_ (zip [start..end-1] (map BS.w2c $ B.unpack bytes)) $ \(i, d) -> writeMemory i (BS.c2w d)
            liftIO $ print "Done"
            liftIO $ hClose h

        _ -> error $ "Unknown OSFILE call " ++ show a ++ "," ++ show x ++ "," ++ show y

