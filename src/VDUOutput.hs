module VDUOutput where

import Control.Monad.IO.Class
import Data.Array
import Data.Word
import Data.ByteString.Internal as BS

extra_bytes_list :: [Int]
extra_bytes_list = [0, 1, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0,
                    0, 1, 2, 5, 0, 0, 1, 9,
                    8, 5, 0, 0, 4, 4, 0, 2]

extra_bytes :: Array Word8 Int
extra_bytes = listArray (0, 31) extra_bytes_list

data VDUOutput = VDUOutput {
                    vbuffer :: [Word8],
                    requiredChars :: Int
                 }


emptyVDUQueue :: VDUOutput
emptyVDUQueue = VDUOutput [] 0
complexVDU :: [Word8] -> IO ()
complexVDU cs = putStr $ show cs

writeOrdinaryChar :: Word8 -> IO ()
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

writeSpecialChar :: Word8 -> IO ()
writeSpecialChar c = 
    case c of
        10 -> liftIO $ putStrLn "\x1b[0m"
        13 -> liftIO $ putStrLn "\x1b[0m"
        _ -> liftIO $ putStr $ "<" ++ show c ++ ">"

writeChar :: Word8 -> VDUOutput -> IO VDUOutput
writeChar c (VDUOutput b n) | n > 1 = return $ VDUOutput (b ++ [c]) (n-1)
writeChar c (VDUOutput b n) | n == 1 = do
    complexVDU (b ++ [c])
    return $ VDUOutput [] 0
writeChar c v@(VDUOutput _ 0) | c < 32 && extra_bytes!c == 0 = do
    writeSpecialChar c
    return v
writeChar c (VDUOutput _ 0) | c < 32 = return $ VDUOutput [c] (extra_bytes!c)
writeChar c v@(VDUOutput _ 0) = do
    writeOrdinaryChar c
    return v
