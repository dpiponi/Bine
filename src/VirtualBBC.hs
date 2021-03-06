{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module VirtualBBC where

import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as BS (c2w, w2c)
import Data.Array.IO
import Control.Monad.State
import VDUOutput
import Control.Lens
import System.IO.Error
import Data.Char
import TraceLog
import Text.Printf
import Data.Bits.Lens
import Utils
import Data.Foldable
import System.Exit
import Data.Bits
import Data.Word
import Numeric
import System.IO
import System.Console.Haskeline
import qualified Data.IntMap as M
import FileSystems

import OSFIND
import OSFILE
import OSWORD
import OSBYTE
import OSCLI
import Core
import State6502
import Monad6502
import VDUOutput

translateKey :: Word8 -> Word8
translateKey 10 = 13
translateKey k = k

instance Emu6502 Monad6502 where
    {-# INLINE readMemory #-}
    readMemory addr = do
        m <- use mem
        liftIO $ readArray m (fromIntegral addr)

    {-# INLINE writeMemory #-}
    writeMemory addr v = do
        m <- use mem
        liftIO $ writeArray m (fromIntegral addr) v

    {-# INLINE getPC #-}
    getPC = use (regs . pc)
    {-# INLINE tick #-}
    tick n = clock += n
    {-# INLINE putC #-}
    putC b = regs . flagC .= b
    {-# INLINE getC #-}
    getC = use (regs . flagC)
    {-# INLINE putZ #-}
    putZ b = regs . flagZ .= b
    {-# INLINE getZ #-}
    getZ = use (regs . flagZ)
    {-# INLINE putI #-}
    putI b = regs . flagI .= b
    {-# INLINE getI #-}
    getI = use (regs . flagI)
    {-# INLINE putD #-}
    putD b = regs . flagD .= b
    {-# INLINE getD #-}
    getD = use (regs . flagD)
    {-# INLINE putB #-}
    putB b = regs . flagB .= b
    {-# INLINE getB #-}
    getB = use (regs . flagB)
    {-# INLINE putV #-}
    putV b = regs . flagV .= b
    {-# INLINE getV #-}
    getV = use (regs . flagV)
    {-# INLINE putN #-}
    putN b = regs . flagN .= b
    {-# INLINE getN #-}
    getN = use (regs . flagN)
    {-# INLINE getA #-}
    getA = use (regs . a)
    {-# INLINE putA #-}
    putA r = regs . a .= r
    {-# INLINE getS #-}
    getS = use (regs . s)
    {-# INLINE putS #-}
    putS r = regs . s .= r
    {-# INLINE getX #-}
    getX = use (regs . x)
    {-# INLINE putX #-}
    putX r = regs . x .= r
    {-# INLINE getP #-}
    getP = use (regs . p)
    {-# INLINE putP #-}
    putP r = regs . p .= r
    {-# INLINE getY #-}
    getY = use (regs . y)
    {-# INLINE putY #-}
    putY r = regs . y .= r
    {-# INLINE putPC #-}
    putPC r = regs . pc .= r
    {-# INLINE addPC #-}
    addPC n = regs . pc += fromIntegral n

    {-# INLINE debugStr #-}
    debugStr _ str = do
        d <- use debug
        if d
            then liftIO $ putStr str
            else return ()

    {-# INLINE debugStrLn #-}
    debugStrLn _ str = do
        d <- use debug
        if d
            then liftIO $ putStrLn str
            else return ()

    {-# INLINE illegal #-}
    illegal i = do
        if i==0x02
            then do
                -- host op
                pp <- getPC
                -- Retroactively fix PC
                putPC $ pp-1
                p0 <- getPC
                op <- readMemory (p0+1)
                --liftIO $ putStrLn $ "Host call with op 0x" ++ showHex op ""
                case op of
                    0x00 -> do
                        liftIO $ exitSuccess
                    -- WRCH
                    0x01 -> do
                        c <- getA
                        queue <- use vduQueue
                        queue' <- writeChar c queue
                        vduQueue .= queue'
                        putPC $ p0+2
                    -- RDCH
                    0x02 -> do
                        tracelog "OSRDCH..."
                        c <- liftIO $ getChar
                        putA (translateKey $ BS.c2w c)
                        tracelog $ printf "...got character %d" c
                        putPC $ p0+2
                    -- CLI
                    0x04 -> do
                        oscli
                        --putPC $ p0+2
                    -- HBYTE
                    0x05 -> do
                        a <- getA
                        x <- getX
                        y <- getY
                        osbyte a x y
                        putPC $ p0+2
                    0x06 -> do
                        osfile
                        putPC $ p0+2
                    0x07 -> do
                        a <- getA
                        x <- getX
                        y <- getY
                        case (y, 0) of

                            --Return current filing system
                            (0, 0) -> do
                                putA 99
                                putD False

                            _ -> error $ "Unknown OSARGS call " ++ show a ++ "," ++ show x ++ "," ++ show y
                    -- OSBGET
                    0x08 -> do
                        a <- getA
                        x <- getX
                        y <- getY
                        tracelog $ printf "OSBGET A=%02x X=%02x Y=%02x" a x y
                        hs <- use handles
                        let k = fromIntegral y
                        let mh = M.lookup k hs
                        case mh of
                            Nothing -> error $ "Unknown handle #" ++ show k
                            Just (HHandle h) -> do
                                ic <- liftIO $ tryIOError (hGetChar h)
                                case ic of
                                    Left e ->
                                        if isEOFError e
                                            then return ()
                                            else do
                                                putC True
                                                putA 0xfe
                                    Right c -> do
                                        putA (BS.c2w c)
                                        putC False
                            Just (BHandle i bs) -> do
                                if i < B.length bs
                                    then do
                                        putA (bs `B.index` i)
                                        handles %= M.insert k (BHandle (i+1) bs)
                                        putC False
                                    else do
                                        putC True
                                        putA 0xfe
                        putPC $ p0+2
                    -- OSBPUT
                    0x09 -> do
                        a <- getA
                        x <- getX
                        y <- getY
                        tracelog $ printf "OSBPUT A=%02x X=%02x Y=%02x" a x y
                        hs <- use handles
                        let k = fromIntegral y
                        let mh = M.lookup k hs
                        let c = BS.w2c (fromIntegral a)
                        case mh of
                            Nothing -> error $ "Unknown handle #" ++ show k
                            Just (HHandle h) -> do
                                ic <- liftIO $ tryIOError (hPutChar h c)
                                case ic of
                                    Left e -> liftIO $ putStrLn $ "Error writing to #" ++ show k
                                    Right _ -> return ()
                            Just (BHandle _ _) -> do
                                liftIO $ putStrLn $ "Error writing to (readonly) #" ++ show k
                        putPC $ p0+2
                    0x0a -> do
                        a <- getA
                        x <- getX
                        y <- getY
                        error $ "Unknown OSGBPB call " ++ show a ++ "," ++ show x ++ "," ++ show y

                    -- OSFIND
                    0x0b -> do
                        osfind
                        putPC $ p0+2

                    0x0c -> do
                        osword
                        putPC $ p0+2

                    otherwise -> do
                        error $ "Unknown host op 0x" ++ showHex op "" ++ " at 0x" ++ showHex p0 ""
            else error $ "Illegal opcode 0x" ++ showHex i ""

{-
{-# INLINE writeRAM #-}
writeRAM :: IOUArray Int Word8 -> Int -> Word8 -> Monad6502 ()
writeRAM m addr b = liftIO $ writeArray m addr b

{-# INLINE readRAM #-}
readRAM :: IOUArray Int Word8 -> Int -> Monad6502 Word8
readRAM m addr = liftIO $ readArray m addr
-}
