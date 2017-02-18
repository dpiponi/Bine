{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import GHC.Exts
import Data.Array.IO
import Data.Word
import FileSystems
import Data.Time.Clock
import Monad6502
import State6502
import Control.Monad.State
import Control.Concurrent.MVar
import qualified MetaData as D
import Control.Lens hiding (noneOf)
import Data.Bits
import Data.Bits.Lens
import qualified Data.IntMap as I
import qualified Data.Map as M
import qualified Data.ByteString as B
import System.IO
import Data.Binary.Get
import Text.Parsec
import Data.Binary
import System.Console.CmdArgs hiding ((+=))
import Numeric
import Control.Monad.Loops
import System.Console.Haskeline
import Core
import Binary
import Intel hiding (hexWord16, fromHex)
import VirtualBBC
import System.Posix.Signals
import qualified Data.ByteString.Internal as BS (c2w, w2c)
--import Vanilla
--import Atari

data Args = Args { file :: String } deriving (Show, Data, Typeable)

clargs :: Args
clargs = Args { file = "WELCOME.IMG" }

getPadded :: B.ByteString -> Int -> Int -> String
getPadded image 0 start = ""
getPadded image n start =
    let p = image `B.index` start
    in if p == 0 || p == 32
          then ""
          else BS.w2c p : getPadded image (n-1) (start+1)

-- Not quite right but probably works XXX
getVolumeName :: B.ByteString -> String
getVolumeName image = getPadded image 8 0x0 ++ getPadded image 4 0x100

-- Deal with Lk. XXX
getFileName :: B.ByteString -> Int -> String
getFileName image i = (BS.w2c $ image `B.index` (8*i+15)) : getPadded image 7 (8*i+8)

i16' :: Integral a => a -> Word16
i16' = fromIntegral

i32' :: Integral a => a -> Word32
i32' = fromIntegral

make16' :: Word8 -> Word8 -> Word16
make16' lo0 hi0 = (i16' hi0 `shift` 8)+i16' lo0

getWord16 :: B.ByteString -> Int -> Word16
getWord16 image i =
    let lo = image `B.index` i
        hi = image `B.index` (i+1)
    in make16' lo hi

getFile :: B.ByteString -> Int -> IO BBCFile
getFile image i = do
    let header = B.drop (0x100+8+8*i) image
    let extraBits = header `B.index` 0x6
    let loadAddress = (i32' $ getWord16 header 0x0)+((i32' extraBits `shift` 14) .&. 0x30000)
    let execAddress = (i32' $ getWord16 header 0x2)+((i32' extraBits `shift` 10) .&. 0x30000)
    let fileLength = (i32' $ getWord16 header 0x4)+((i32' extraBits `shift` 12) .&. 0x30000)
    let startSector = (i32' $ header `B.index` 7)+(((i32' (header `B.index` 6)) `shift` 8) .&. 0x300)
    let addr = 0x100*startSector
    putStrLn $ "Load address = " ++ showHex loadAddress ""
    putStrLn $ "Exec address = " ++ showHex execAddress ""
    putStrLn $ "Start address = " ++ showHex addr ""
    putStrLn $ "Length = " ++ showHex fileLength ""
    return $ BBCFile loadAddress execAddress (B.take (fromIntegral fileLength) (B.drop (fromIntegral addr) image))

-- http://beebwiki.mdfs.net/Acorn_DFS_disc_format
unpack :: String -> IO ()
unpack filename = do
    handle <- openFile filename ReadMode
    image <- B.hGetContents handle

    let volumeName = getVolumeName image 
    liftIO $ putStrLn $ "Volume name: " ++ volumeName

    let numFiles = fromIntegral $ (image `B.index` 0x105) `shift` (-3)
    liftIO $ putStrLn $ "# files = " ++ show numFiles

    fsdata <- forM [0..(numFiles-1)] $
         \i -> do
            let urname = getFileName image i
            putStrLn $ "urname = '" ++ urname ++ "'"
            let name = head urname : '.' : (D.trimSpaces $ tail urname)
            putStrLn $ "filename = " ++ name
            BBCFile load exec bytes <- getFile image i
            k <- openFile name WriteMode
            B.hPutStr k bytes
            hClose k
            D.setMetaData name load exec

            --return (name, f)

    hClose handle

{-
loadfs :: IO FileSystem
--fs = Host
-- fs = Bytes 0xe00 0xe00 $ M.insert "FILE" (pack (Prelude.map BS.c2w "FRED")) M.empty
--loadfs = loadImage "WELCOME.IMG"
loadfs = loadImage "PQUEST.IMG"
-}

main :: IO ()
main = do
    args <- cmdArgs clargs
    print args

    let filename = file args
    unpack filename
