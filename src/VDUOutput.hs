{-# LANGUAGE FlexibleContexts #-}

module VDUOutput where

import Control.Monad.IO.Class
import Control.Monad.State
import Core
import Data.Array
import Data.ByteString.Internal as BS
import Data.List
import Data.Bits
import Data.Int
import TraceLog
import Data.Word
import Monad6502
import State6502
import Text.Printf

-- VDU codes: http://beebwiki.mdfs.net/VDU
-- ANSI codes: https://en.wikipedia.org/wiki/ANSI_escape_code
-- Block codes: http://www.riscos.com/support/developers/bbcbasic/part2/teletext.html
braille :: String
braille = "⠀⠁⠂⠃⠄⠅⠆⠇⠈⠉⠊⠋⠌⠍⠎⠏⠐⠑⠒⠓⠔⠕⠖⠗⠘⠙⠚⠛⠜⠝⠞⠟⠠⠡⠢⠣⠤⠥⠦⠧⠨⠩⠪⠫⠬⠭⠮⠯⠰⠱⠲⠳⠴⠵⠶⠷⠸⠹⠺⠻⠼⠽⠾⠿"

flipBits :: (Bits a, Num a) => a -> a
flipBits i = (if testBit i 1 then 1 else 0) .|.
             (if testBit i 2 then 8 else 0) .|.
             (if testBit i 4 then 2 else 0) .|.
             (if testBit i 8 then 16 else 0) .|.
             (if testBit i 16 then 4 else 0) .|.
             (if testBit i 64 then 32 else 0)

--reordered_braille :: String
--reordered_braille = [braile !! flip i | i <- [0..63]]

extra_bytes_list :: [Int]
extra_bytes_list = [0, 1, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0,
                    0, 1, 2, 5, 0, 0, 1, 9,
                    8, 5, 0, 0, 4, 4, 0, 2]

extra_bytes :: Array Word8 Int
extra_bytes = listArray (0, 31) extra_bytes_list

emptyVDUQueue :: VDUOutput
emptyVDUQueue = VDUOutput [] 0

complexVDU :: (MonadState State6502 m, Emu6502 m) => [Word8] -> m ()
complexVDU [18, a, b] = tracelog $ printf " (Define graphics colour %d, %d)" a b
complexVDU [22, m] = do
    liftIO $ putStrLn "\x1b[2J\x1b[;H"
    tracelog $ printf " (MODE %d)" m
complexVDU (23 : c : cs) = tracelog $ printf " (Redefine character %d: %s)" c (show cs)
complexVDU [25, k, xlo, xhi, ylo, yhi] =
    tracelog $ printf " (PLOT %d, %d, %d)"
                      k (fromIntegral (make16 xlo xhi) :: Int16)
                        (fromIntegral (make16 ylo yhi) :: Int16)
complexVDU [28, a, b, c, d] =
    tracelog $ printf " (Define text window %d %d %d %d)" a b c d
complexVDU [31, x, y] = do
    tracelog $ printf " Move cursor to %d, %d" x y
    liftIO $ printf "\x1b[%d;%dH" (y+1) (x+1)
complexVDU cs = liftIO $ putStr $ show cs

writeOrdinaryChar :: (MonadState State6502 m, Emu6502 m) => Word8 -> m ()
writeOrdinaryChar c = 
    case c of
        127 -> liftIO $ putStr "\b \b"

        -- Red
        129 -> liftIO $ putStr " \x1b[31m"
        -- Green
        130 -> liftIO $ putStr " \x1b[32m"
        -- Yellow
        131 -> liftIO $ putStr " \x1b[33m"
        -- Blue
        132 -> liftIO $ putStr " \x1b[34m"
        -- Magenta
        133 -> liftIO $ putStr " \x1b[35m"
        -- Cyan
        134 -> liftIO $ putStr " \x1b[36m"
        -- White
        135 -> liftIO $ putStr " \x1b[37m"

        -- Graphics Red
        145 -> liftIO $ putStr " \x1b[31m"
        -- Graphics Green
        146 -> liftIO $ putStr " \x1b[32m"
        -- Yellow
        147 -> liftIO $ putStr " \x1b[33m"
        -- Graphics Blue
        148 -> liftIO $ putStr " \x1b[34m"
        -- Graphics Magenta
        149 -> liftIO $ putStr " \x1b[35m"
        -- Graphics Cyan
        150 -> liftIO $ putStr " \x1b[36m"
        -- Graphics White
        151 -> liftIO $ putStr " \x1b[37m"

        _ -> if c >= 160 && c < 192 || c >= 224
                then liftIO $ putChar (braille !! (fromIntegral $ flipBits (c-160)))
                else liftIO $ putChar (BS.w2c c)

writeSpecialChar :: (MonadState State6502 m, Emu6502 m) => Word8 -> m ()
writeSpecialChar c = 
    case c of
        0 -> return ()
        10 -> liftIO $ putStrLn "\x1b[0m"
        12 -> liftIO $ putStrLn "\x1b[2J\x1b[;H"
        13 -> liftIO $ putStrLn "\x1b[0m"
        14 -> tracelog " (Paged mode on)"
        15 -> tracelog " (Paged mode off)"
        16 -> tracelog " (Clear graphics area)"
        20 -> tracelog " (Restore default logical colours)"
        30 -> liftIO $ putStr "\x1b[;H"
        _ -> tracelog $ printf " (VDU %d)" c

writeChar :: (MonadState State6502 m, Emu6502 m) => Word8 -> VDUOutput -> m VDUOutput
writeChar c (VDUOutput b n) | n > 1 = return $ VDUOutput (b ++ [c]) (n-1)
writeChar c (VDUOutput b n) | n == 1 = do
    tracelog $ printf "VDU %s" (intercalate ", " (map show (b ++ [c])))
    complexVDU (b ++ [c])
    return $ VDUOutput [] 0
writeChar c v@(VDUOutput _ 0) | c < 32 && extra_bytes!c == 0 = do
    tracelog $ printf "VDU %d" c
    writeSpecialChar c
    return v
writeChar c (VDUOutput _ 0) | c < 32 = return $ VDUOutput [c] (extra_bytes!c)
writeChar c v@(VDUOutput _ 0) = do
    writeOrdinaryChar c
    return v
