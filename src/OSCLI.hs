{-# LANGUAGE FlexibleContexts #-}

module OSCLI where

import Control.Monad.State
import System.IO
import System.Process
import Utils
import TraceLog
import Text.Printf
import Core
import KeyInput
import Data.Char
import System.Exit
import MonadInput
import Control.Lens hiding (noneOf)
import State6502
import Monad6502
import Utils
import Core
import qualified Data.IntMap as M
import qualified Data.ByteString.Internal as BS (c2w, w2c)
import Data.Word
import Numeric
import Data.Bits
import Text.Parsec
import OSBYTE
import OSFILE

data Command = FX Int Int Int
             | LOAD String Int -- <-- XXX needs to me Maybe Int
             | SAVE String Int Bool Int Int Int
             | RUN String -- XXX pass args
             | KEY Int String
             | DIR Char
             | EXIT Int
             | TAPE Int deriving Show

decimal :: ParsecT String u Identity Int
decimal = do
    digits <- many1 digit
    return $ read digits

number :: Stream s m t => Int -> ParsecT s u m Char -> ParsecT s u m Int
number base baseDigit
    = do{ digits <- many1 baseDigit
        ; let n = foldl (\x d -> base*x + (digitToInt d)) 0 digits
        ; seq n (return n)
        }

filename :: ParsecT String u Identity String
filename = (char '"' >> (many1 (noneOf "\"") <* char '"'))
           <|> many1 (noneOf " ")

ignoreCase :: Stream s m Char => [Char] -> ParsecT s u m [Char]
ignoreCase [] = return []
ignoreCase (c : cs) | isUpper c = do
    m <- char (toLower c) <|> char c
    ms <- ignoreCase cs
    return (m : ms)
ignoreCase (c : cs) | isLower c = do
    m <- char '.' <|> char c <|> char (toUpper c)
    if m == '.'
        then
            return "."
        else do
            ms <- ignoreCase cs
            return (m : ms)
ignoreCase (c : cs) = do
    m <- char c
    ms <- ignoreCase cs
    return (m : ms)

-- XXX Ignore case of commands
parseCommand :: ParsecT String u Identity Command
parseCommand = (FX <$> (ignoreCase "FX" >> spaces >> decimal)
                   <*> option 0 (spaces >> char ',' >> spaces >> decimal)
                   <*> option 0 (spaces >> char ',' >> spaces >> decimal))
               <|>
               (TAPE <$> (ignoreCase "TAPE" >> spaces >> option 0 decimal))
               <|>
               (LOAD <$> (ignoreCase "Load" >> spaces >> filename)
                     <*> option 0 (spaces >> number 16 hexDigit))
               <|>
               (RUN <$> (ignoreCase "Run" >> spaces >> (filename <* spaces)))
               <|>
               (KEY <$> (ignoreCase "KEY" >> spaces >> decimal)
                    <*> many anyChar)
               <|>
               (DIR <$> (ignoreCase "DIR" >> spaces >> anyChar))
               <|>
               (SAVE <$> (ignoreCase "Save" >> spaces >> (filename <* spaces))
                     <*> (number 16 hexDigit <* spaces)
                     <*> option False (char '+' >> spaces >> return True)
                     <*> (number 16 hexDigit <* spaces)
                     <*> option 0 (number 16 hexDigit <* spaces)
                     <*> option 0 (number 16 hexDigit <* spaces))
                <|>
                (ignoreCase "EXit" >> spaces >> ((EXIT <$> option 0 decimal) <* spaces))

execStarCommand :: (Emu6502 m, MonadState State6502 m) => Command -> m ()
execStarCommand (EXIT 0) = liftIO $ exitSuccess
execStarCommand (EXIT n) = liftIO $ exitWith (ExitFailure n)
execStarCommand (FX a x y) = do
    osbyte (i8 a) (i8 x) (i8 y)
    p0 <- getPC
    putPC $ p0+2
execStarCommand (KEY key def) = do
    defineKey key (map BS.c2w def)
    p0 <- getPC
    putPC $ p0+2
execStarCommand (LOAD filename loadAddress) = do
    putA 0xff
    -- Control block at &02EE
    putX 0xee
    putY 0x02
    let addrFilename = 0x200 :: Word16
    -- Write filename
    forM_ (zip [addrFilename..] filename) $
            \(i, d) -> writeMemory (fromIntegral i) (BS.c2w d)
    -- Terminate filename with zero
    writeMemory (fromIntegral addrFilename+fromIntegral (length filename)) 0
    -- Write address of filename
    writeWord16 0x2ee addrFilename
    -- Set load address
    if loadAddress == 0
        then
            -- Use address in file
            writeWord32 (0x2ee+6) (fromIntegral 1)
        else do
            -- Use user specified address
            writeWord32 (0x2ee+6) (fromIntegral 0)
            writeWord32 (0x2ee+2) (fromIntegral loadAddress)
    osfile
    p0 <- getPC
    putPC $ p0+2
execStarCommand (SAVE filename startAddress relative endAddress execAddress reloadAddress) = do
    --liftIO $ printf "*SAVE %s %08x %d %08x %08x" filename startAddress (if relative then 1 else 0::Int) endAddress execAddress
    putA 0
    -- Control block at &02EE
    putX 0xee
    putY 0x02
    let addrFilename = 0x200 :: Word16
    -- Write filename
    forM_ (zip [addrFilename..] filename) $
            \(i, d) -> writeMemory (fromIntegral i) (BS.c2w d)
    -- Terminate filename with zero
    writeMemory (fromIntegral addrFilename+fromIntegral (length filename)) 0
    -- Write address of filename
    writeWord16 0x2ee addrFilename
    writeWord32 (0x2ee+2) (fromIntegral $ if reloadAddress == 0 then startAddress else reloadAddress)
    writeWord32 (0x2ee+6) (fromIntegral execAddress)
    writeWord32 (0x2ee+0xa) (fromIntegral startAddress)
    writeWord32 (0x2ee+0xe) (fromIntegral $ if relative then startAddress+endAddress else endAddress)
    osfile
    p0 <- getPC
    putPC $ p0+2
execStarCommand (RUN filename) = do
    putA 0xff
    -- Control block at &02EE
    putX 0xee
    putY 0x02
    let addrFilename = 0x200 :: Word16
    forM_ (zip [addrFilename..] filename) $ \(i, d) -> writeMemory (fromIntegral i) (BS.c2w d)
    writeMemory (fromIntegral addrFilename+fromIntegral (length filename)) 0
    writeWord16 0x2ee addrFilename
    -- Signal that we want to use load address alreday in file.
    writeWord32 (0x2ee+6) 1
    osfile
    fileExec <- word32At (0x2ee+6)
    liftIO $ putStrLn $ "Executing from 0x" ++ showHex fileExec ""
    -- Fake JSR
    p0 <- getPC
    push $ hi (p0+1)
    push $ lo (p0+1)
    putPC (i16 fileExec)
execStarCommand (DIR dirname) = do
    currentDirectory .= dirname
    p0 <- getPC
    putPC $ p0+2
execStarCommand (TAPE t) = do
    liftIO $ putStrLn $ "*TAPE " ++ show t
    p0 <- getPC
    putPC $ p0+2

{-# INLINABLE oscli #-}
oscli :: (MonadState State6502 m, Emu6502 m) => m ()
oscli = do
    lo <- getX
    hi <- getY
    let addr = make16 lo hi
    cmd <- stringAt addr
    tracelog $ printf "OSCLI: %s" cmd
    let cmd' = removeStars cmd
    let cmd'' = parse parseCommand "" cmd'
    case cmd'' of
        Right cmd''' -> execStarCommand cmd'''
        Left _ -> do
            void $ liftIO $ system cmd'
            p0 <- getPC
            putPC $ p0+2
    return ()
