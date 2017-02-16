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
import Control.Lens hiding (noneOf)
import Data.Bits
import Data.Bits.Lens
import qualified Data.IntMap as I
import qualified Data.Map as M
import Data.ByteString as B hiding (putStrLn, putStr, count, head)
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
import KeyInput
import qualified Data.ByteString.Internal as BS (c2w, w2c)
--import Vanilla
--import Atari

data Args = Args { verbose :: Bool,
                   file :: String,
                   org :: String,
                   entry :: String } deriving (Show, Data, Typeable)

clargs :: Args
clargs = Args { verbose = False, org = "0", entry = "0", file = "test.bin" }

times :: (Integral n, Monad m) => n -> m a -> m ()
times 0 _ = return ()
times n m = m >> times (n-1) m

data FileSpec = Intel String | Binary String Word16 deriving Show

hexWord16 :: Stream s m Char => ParsecT s u m Word16
hexWord16 = fromHex <$> count 4 hexDigit

fromHex :: (Num a, Eq a) => String -> a
fromHex = fst . head . readHex

filespec = do
    a <- anyChar
    case a of
        'i' -> do
            char ':'
            filename <- many (noneOf ",")
            return (Intel filename)
        'b' -> do
            char ':'
            filename <- many (noneOf ":")
            char ':'
            address <- hexWord16
            return (Binary filename address)

filespecs = filespec `sepBy1` (char ',')

--xxx = getPC

loadFile :: IOUArray Int Word8 -> FileSpec -> IO ()
loadFile arr (Intel f) = readIntel arr f
loadFile arr (Binary f o) = readBinary arr f o

handler interrupted = do
    print "SIGINT"
    putMVar interrupted 1

{-
loadfs :: IO FileSystem
--fs = Host
-- fs = Bytes 0xe00 0xe00 $ M.insert "FILE" (pack (Prelude.map BS.c2w "FRED")) M.empty
--loadfs = loadImage "WELCOME.IMG"
loadfs = loadImage "PQUEST.IMG"
-}

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    --hSetEcho stdin False
    -- hSetEcho stdin False
    args <- cmdArgs clargs
    print args

    arr <- newArray (0, 0xffff) 0 :: IO (IOUArray Int Word8)

    putStrLn $ "Running from " ++ file args
    let Right specs = parse filespecs "" (file args)
    forM_ specs $ \spec ->
        loadFile arr spec
        
    let [(entryPoint, _)] = readHex (entry args)
    systime <- getCurrentTime
    --fs <- loadfs
    let state = S { _mem = arr,  _clock = 0, _regs = R entryPoint 0 0 0 0 0xff,
                    _debug = verbose args, _handles = I.empty, _sysclock = systime,
                    _currentDirectory = '$', _keyQueue = emptyQueue }
    
    interrupted <- newEmptyMVar :: IO (MVar Int)
    installHandler sigINT (Catch $ handler interrupted) Nothing

    putStrLn $ "Executing from 0x" ++ showHex entryPoint ""
    --flip execStateT state $ unM $ forever (inline step)
    runInputT defaultSettings $ flip execStateT state $ unM $
        forever $ do
            i <- liftIO $ isEmptyMVar interrupted
            when (not i) $ do
                writeMemory 0xff 0x80
                liftIO $ putStrLn "ESCAPE!!!!!!!!!!!!!!!!!!!!!"
                _ <- liftIO $ takeMVar interrupted
                return ()
            --liftIO $ print "X"
            step

    return ()
