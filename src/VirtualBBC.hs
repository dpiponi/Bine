{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module VirtualBBC where

import qualified Data.ByteString.Internal as BS (c2w, w2c)
import Data.Array.IO
import Control.Monad.State
import Control.Lens
import System.IO.Error
import Data.Bits.Lens
import System.Exit
import Data.Bits
import System.Process
import Data.Word
import Numeric
import System.IO
import System.Console.Haskeline
import qualified Data.IntMap as M

import Core

data Registers = R {
    _pc :: !Word16,
    _p :: !Word8,
    _a :: !Word8,
    _x :: !Word8,
    _y :: !Word8,
    _s :: !Word8
}

makeLenses ''Registers

{-# INLINE flagC #-}
flagC :: Lens' Registers Bool
flagC = p . bitAt 0

{-# INLINE flagZ #-}
flagZ :: Lens' Registers Bool
flagZ = p . bitAt 1

{-# INLINE flagI #-}
flagI :: Lens' Registers Bool
flagI = p . bitAt 2

{-# INLINE flagD #-}
flagD :: Lens' Registers Bool
flagD = p . bitAt 3

{-# INLINE flagB #-}
flagB :: Lens' Registers Bool
flagB = p . bitAt 4

{-# INLINE flagV #-}
flagV :: Lens' Registers Bool
flagV = p . bitAt 6

{-# INLINE flagN #-}
flagN :: Lens' Registers Bool
flagN = p . bitAt 7

data State6502 = S {
    _mem :: IOUArray Int Word8,
    _clock :: !Int,
    _regs :: !Registers,
    _debug :: !Bool,

    _handles :: M.IntMap Handle
}

makeLenses ''State6502

removeStars :: String -> String
removeStars ('*' : cs) = removeStars cs
removeStars cs = cs

freeHandle :: M.IntMap Handle -> Int
freeHandle hs =
    let freeHandle' i = if i `M.notMember` hs then i else freeHandle' (i+1)
    in freeHandle' 1

newtype Monad6502 a = M { unM :: StateT State6502 (InputT IO) a }
    deriving (Functor, Applicative, Monad, MonadState State6502, MonadIO)

i32 :: Integral a => a -> Word32
i32 = fromIntegral

{-# INLINE make32 #-}
make32 :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
make32 b0 b1 b2 b3 = (i32 b3 `shift` 24)+(i32 b2 `shift` 32)+(i32 b1 `shift` 8)+i32 b0

stringAt :: Word16 -> Monad6502 String
stringAt addr = do
    let loop cmd i = do
                    byte <- readMemory (addr+i16 i)
                    if byte /= 0x0d
                        then loop (cmd ++ [BS.w2c byte]) (i+1)
                        else return cmd
    loop "" 0

word16At :: Word16 -> Monad6502 Word16
word16At addr = do
    lo <- readMemory addr
    hi <- readMemory (addr+1)
    return $ make16 lo hi

word32At :: Word16 -> Monad6502 Word32
word32At addr = do
    b0 <- readMemory addr
    b1 <- readMemory (addr+1)
    b2 <- readMemory (addr+2)
    b3 <- readMemory (addr+3)
    return $ make32 b0 b1 b2 b3

putWord32 :: Word16 -> Word32 -> Monad6502 ()
putWord32 addr w = do
    writeMemory addr (i8 $ w)
    writeMemory (addr+1) (i8 (w `shift` (-8)))
    writeMemory (addr+2) (i8 (w `shift` (-16)))
    writeMemory (addr+3) (i8 (w `shift` (-24)))

osfile :: Monad6502 ()
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

osfind :: Monad6502 ()
osfind = do
    a <- getA
    x <- getX
    y <- getY
    case a of
        0x00 -> do
            when (y == 0) $ error "Close all files unimplemented"
            hs <- use handles
            let k = fromIntegral y
            let mh = M.lookup k hs
            case mh of
                Nothing -> error $ "Unknown handle #" ++ show k
                Just h -> do
                    liftIO $ putStrLn $ "Closing #" ++ show k
                    liftIO $ hClose h
                    handles %= M.delete k

        0x80 -> do
            let addr = make16 x y
            filename <- stringAt addr
            h <- liftIO $ openFile filename WriteMode
            hs <- use handles
            let k = freeHandle hs
            handles %= M.insert k h
            liftIO $ putStrLn $ "Opened " ++ filename ++ " with handle " ++ show k
            putA (i8 k)

        _ -> liftIO $ putStrLn $ "Unknown OSFIND call: " ++ show a ++ "," ++ show x ++ "," ++ show y

instance Emu6502 Monad6502 where
    {-# INLINE readMemory #-}
    readMemory addr = do
        -- debugStrLn $ "Reading from addr " ++ showHex addr ""
        -- if addr == 0x8000
        --     then do
        --         c <- liftIO $ getChar
        --         return $ BS.c2w c
        --     else do
        --         m <- use mem
        --         liftIO $ readArray m addr

        m <- use mem
        liftIO $ readArray m (fromIntegral addr)

    {-# INLINE writeMemory #-}
    writeMemory addr v = do
        m <- use mem
        liftIO $ writeArray m (fromIntegral addr) v
        -- debugStrLn $ "Writing " ++ showHex v "" ++ " to addr " ++ showHex addr ""
        -- if addr == 0x8000
        --     then do
        --         liftIO $ putChar (BS.w2c v)
        --     else do
        --         m <- use mem
        --         liftIO $ writeArray m addr v

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
                putPC $ pp-1
                p0 <- getPC
                op <- readMemory (p0+1)
                --liftIO $ putStrLn $ "Host call with op 0x" ++ showHex op ""
                case op of
                    0x00 -> do
                        liftIO $ exitSuccess
                    0x01 -> do
                        c <- getA
                        liftIO $ putChar (BS.w2c c)
                        putPC $ p0+2
                    0x02 -> do
                        c <- liftIO $ getChar
                        putA (BS.c2w c)
                        putPC $ p0+2
                    0x03 -> do
                        lo <- getX
                        hi <- getY
                        let addr = make16 lo hi
                        sAddrLo <- readMemory addr
                        sAddrHi <- readMemory (addr+1)
                        let sAddr = make16 sAddrLo sAddrHi
                        --line <- liftIO $ getLine
                        Just line <- M $ lift $ getInputLine ""
                        let n = length line
                        forM_ [0..n-1] $ \i -> do
                            writeMemory (sAddr+i16 i) (BS.c2w (line!!i))
                        writeMemory (sAddr+i16 n) 13
                        putC False
                        putY $ i8 n+1
                        putPC $ p0+2
                    -- CLI
                    0x04 -> do
                        lo <- getX
                        hi <- getY
                        let addr = make16 lo hi
                        {-
                        let loop cmd i = do
                                        byte <- readMemory (addr+i16 i)
                                        if byte /= 0x0d
                                            then loop (cmd ++ [BS.w2c byte]) (i+1)
                                            else return cmd
                        cmd <- loop "" 0
                        -}
                        cmd <- stringAt addr
                        let cmd' = removeStars cmd
                        liftIO $ putStrLn cmd'
                        liftIO $ system cmd'
                        putPC $ p0+2
                    -- HBYTE
                    0x05 -> do
                        a <- getA
                        x <- getX
                        y <- getY
                        case a of
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
                            _ -> do
                                error $ "Unknown OSBYTE call " ++ show a ++ "," ++ show x ++ "," ++ show y
                        putPC $ p0+2
                    0x06 -> do
                        osfile
                        putPC $ p0+2
                    0x07 -> do
                        a <- getA
                        x <- getX
                        y <- getY
                        error $ "Unknown OSARGS call " ++ show a ++ "," ++ show x ++ "," ++ show y
                    -- OSBGET
                    0x08 -> do
                        a <- getA
                        x <- getX
                        y <- getY
                        hs <- use handles
                        let k = fromIntegral y
                        let mh = M.lookup k hs
                        case mh of
                            Nothing -> error $ "Unknown handle #" ++ show k
                            Just h -> do
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
                        putPC $ p0+2
                    -- OSBPUT
                    0x09 -> do
                        a <- getA
                        x <- getX
                        y <- getY
                        hs <- use handles
                        let k = fromIntegral y
                        let mh = M.lookup k hs
                        let c = BS.w2c (fromIntegral a)
                        case mh of
                            Nothing -> error $ "Unknown handle #" ++ show k
                            Just h -> do
                                ic <- liftIO $ tryIOError (hPutChar h c)
                                case ic of
                                    Left e -> liftIO $ putStrLn $ "Error writing to #" ++ show k
                                    Right _ -> return ()
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
