{-# LANGUAGE FlexibleContexts #-}

module VDUOutput where

import Control.Monad.IO.Class
import Control.Monad.State
import Core
import Data.Array
import Data.ByteString.Internal as BS
import Data.List
import Data.Int
import TraceLog
import Data.Word
import Monad6502
import State6502
import Text.Printf

-- VDU codes: http://beebwiki.mdfs.net/VDU
-- ANSI codes: https://en.wikipedia.org/wiki/ANSI_escape_code

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
        -- Blue
        131 -> liftIO $ putStr " \x1b[32m"
        132 -> liftIO $ putStr " \x1b[33m"
        133 -> liftIO $ putStr " \x1b[34m"
        134 -> liftIO $ putStr " \x1b[35m"
        135 -> liftIO $ putStr " \x1b[36m"
        -- Flash
        --136 -> liftIO $ putStr " \x1b[5m"
        --137 flash off
        _ -> liftIO $ putChar (BS.w2c c)

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
