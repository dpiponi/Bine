{-# LANGUAGE FlexibleContexts #-}

module OSCLI where

import Control.Monad.State
import System.IO
import System.Process
import Utils
import Data.Char
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
             | LOAD String Int deriving Show

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

-- XXX Ignore case of commands
parseCommand :: ParsecT String u Identity Command
parseCommand = (FX <$> do
                            (string "fx" >> spaces >> decimal)
                            <*> option 0 (spaces >> char ',' >> spaces >> decimal)
                            <*> option 0 (spaces >> char ',' >> spaces >> decimal))
               <|>
               (LOAD <$> (string "load" >> spaces >> filename) <*> option 0 (spaces >> number 16 hexDigit))

writeWord16 :: Emu6502 m => Word16 -> Word16 -> m ()
writeWord16 i w = do
    writeMemory i (fromIntegral w)
    writeMemory (i+1) (fromIntegral $ w `shift` (-8))

writeWord32 :: Emu6502 m => Word16 -> Word32 -> m ()
writeWord32 i w = do
    writeMemory i (fromIntegral w)
    writeMemory (i+1) (fromIntegral $ w `shift` (-8))
    writeMemory (i+2) (fromIntegral $ w `shift` (-16))
    writeMemory (i+3) (fromIntegral $ w `shift` (-24))

execStarCommand :: (Emu6502 m, MonadState State6502 m) => Command -> m ()
execStarCommand (FX a x y) = osbyte (i8 a) (i8 x) (i8 y)
execStarCommand (LOAD filename loadAddress) = do
    putA 0xff
    -- Control block at &02EE
    putX 0xee
    putY 0x02
    let addrFilename = 0x200 :: Word16
    forM_ (zip [addrFilename..] filename) $ \(i, d) -> writeMemory (fromIntegral i) (BS.c2w d)
    writeMemory (fromIntegral addrFilename+fromIntegral (length filename)) 0
    writeWord16 0x2ee addrFilename
    writeWord32 (0x2ee+2) (fromIntegral loadAddress)
    -- writeWord32 (0x2ee+6) execAddress
    -- writeWord32 (0x2ee+10) saveStart
    -- writeWord32 (0x2ee+14) saveEnd
    osfile

{-# INLINABLE oscli #-}
oscli :: (MonadState State6502 m, Emu6502 m) => m ()
oscli = do
    lo <- getX
    hi <- getY
    let addr = make16 lo hi
    cmd <- stringAt addr
    let cmd' = removeStars cmd
    liftIO $ putStrLn cmd'
    let cmd'' = parse parseCommand "" cmd'
    case cmd'' of
        Right cmd''' -> execStarCommand cmd'''
        Left _ -> void $ liftIO $ system cmd'
    return ()
