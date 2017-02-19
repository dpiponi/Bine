{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import GHC.Exts
import Data.Array.IO
import System.Directory
import Data.Word
import FileSystems
import Data.Time.Clock
import System.Environment
import Monad6502
import Text.Printf
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
import VDUOutput
import TraceLog
import qualified Data.ByteString.Internal as BS (c2w, w2c)
--import Vanilla
--import Atari

data Args = Args { verbose :: Bool,
                   file :: Maybe String,
                   org :: String,
                   entry :: String,
                   logfile :: Maybe String,
                   directory :: Maybe String } deriving (Show, Data, Typeable)

clargs :: Args
clargs = Args { verbose = False, org = "0", entry = "C000", file = Nothing,
                logfile = Nothing, directory = Nothing }

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

-- b:../../roms/BASIC-1.0:8000,b:../../os.bin:c000

{-
getROMSpec :: IO (Maybe (String, String))
getROMSpec = do
    case lookupEnv "BBC_LANGUAGE" of
        Nothing -> return Nothing
        Just languageRom -> case (lookupEnv "BBC_ROM") of
            case lookupEnv "BBC_ROM" of
            Nothing -> return Nothing
            Just osRom -> return (languageRom, osRom)
            -}

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    --hSetEcho stdin False
    -- hSetEcho stdin False
    args <- cmdArgs clargs
    --print args
    putStrLn "BBC Computer 32K\n"

    arr <- newArray (0, 0xffff) 0 :: IO (IOUArray Int Word8)

    mSpecString <- case file args of
                        Nothing -> lookupEnv "BBC_ROMS"
                        Just specString' -> return $ Just specString'

    case mSpecString of
        Nothing -> return ()
        Just specString -> do
            let mSpecs = parse filespecs "" specString
            case mSpecs of
                Right specs -> forM_ specs $ \spec -> loadFile arr spec
                Left _ -> putStrLn $ "Unable to parse ROM specification: " ++ show specString

    logHandle <- case logfile args of
        Nothing -> return Nothing
        Just logName -> do
            handle <- openFile logName WriteMode
            return $ Just handle
        
    let [(entryPoint, _)] = readHex (entry args)
    systime <- getCurrentTime
    let state = S { _mem = arr,  _clock = 0, _regs = R entryPoint 0 0 0 0 0xff,
                    _debug = verbose args, _handles = I.empty, _sysclock = systime,
                    _currentDirectory = '$', _keyQueue = emptyQueue,
                    _vduQueue = emptyVDUQueue, _logFile = logHandle }

    case directory args of
        Nothing -> return ()
        Just d -> setCurrentDirectory d
    
    interrupted <- newEmptyMVar :: IO (MVar Int)
    installHandler sigINT (Catch $ handler interrupted) Nothing

    --flip execStateT state $ unM $ forever (inline step)
    runInputT defaultSettings $ flip execStateT state $ unM $ do
        tracelog $ printf "\nExecuting from address %04x" entryPoint
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
