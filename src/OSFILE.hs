{-# LANGUAGE FlexibleContexts #-}

module OSFILE where

import FileSystems
import Text.Printf
import TraceLog
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

hostFileName :: (MonadState State6502 m, Emu6502 m) => String -> m String
hostFileName name@(_ : '.' : _) = return name
hostFileName name = do
    dir <- use currentDirectory
    return $ dir : '.' : name

saveBlock :: (MonadState State6502 m, Emu6502 m) => Word16 -> String -> m ()
saveBlock blockAddr hostName = do
    startData32 <- word32At (blockAddr+0xa)
    endData32 <- word32At (blockAddr+0xe)
    let start = i16 startData32
    let end = i16 endData32
    tracelog $ printf " Save %04x:%04x to '%s'" start end hostName
    h <- liftIO $ openBinaryFile hostName WriteMode
    forM_ [start..end] $ \i -> do
        x <- readMemory i
        liftIO $ hPutChar h (BS.w2c x)
    liftIO $ hClose h

loadFile :: (MonadState State6502 m, Emu6502 m) => Word16 -> String -> m ()
loadFile blockAddr hostName = do
    loadAddr32 <- word32At (blockAddr+0x2)
    execAddr32 <- word32At (blockAddr+0x6)
    startData32 <- word32At (blockAddr+0xa)
    addressType <- readMemory (blockAddr+0x6)
    (fileLoad, fileExec) <- liftIO $ getMetaData hostName
    -- If caller-specified execution address ends in zero
    -- use user-specified load address
    -- otherwise use load address in file
    let start = if addressType == 0
        then fromIntegral loadAddr32
        else fromIntegral fileLoad
    writeWord32 (blockAddr+0x6) fileExec
    h <- liftIO $ openBinaryFile hostName ReadMode
    bytes <- liftIO $ B.hGetContents h
    let len = B.length bytes
    let end = start+fromIntegral len
    tracelog $ printf " Load %04x:%04x from '%s'" start end hostName
    forM_ (zip [start..end-1] (map BS.w2c $ B.unpack bytes)) $ \(i, d) -> writeMemory i (BS.c2w d)
    liftIO $ print "Done"
    liftIO $ hClose h

-- http://mdfs.net/Docs/Comp/BBC/API/OSFILE.htm
{-# INLINABLE osfile #-}
osfile :: (MonadState State6502 m, Emu6502 m) => m ()
osfile = do
    a <- getA
    x <- getX
    y <- getY

    tracelog $ printf "OSFILE A=%02x X=%02x Y=%02x" a x y

    let blockAddr = make16 x y
    stringAddr <- word16At blockAddr
    rawFilename <- stringAt stringAddr
    hostName <- hostFileName rawFilename
    
    -- Note that the 'end' field points to the last byte,
    -- not the first byte after the end.
    case a of
        0x00 -> saveBlock blockAddr hostName
        0xff -> loadFile blockAddr hostName

        _ -> error $ "Unknown OSFILE call " ++ show a ++ "," ++ show x ++ "," ++ show y

