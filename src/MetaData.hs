module MetaData where

import Control.Monad.State
import System.IO
import Data.Char
import Control.Lens hiding (noneOf)
import qualified Data.Map as M
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as BS
import System.IO.Error
import Data.Word
import Text.Printf
import Text.Parsec

data MetaData = MetaData {
                    loadAddress :: Word32,
                    execAddress :: Word32
                } deriving Show

type MetaCatalogue = M.Map String MetaData

number :: Stream s m t => Int -> ParsecT s u m Char -> ParsecT s u m Int
number base baseDigit =
    foldl (\x d -> base*x + (digitToInt d)) 0 <$> many1 baseDigit

hexadecimal :: ParsecT String u Identity Int
hexadecimal = number 16 hexDigit

trimSpaces :: String -> String
trimSpaces [] = []
trimSpaces (' ' : _) = []
trimSpaces (c : cs) = c : trimSpaces cs

fileName :: ParsecT String u Identity String
fileName = trimSpaces <$> count 7 (noneOf "#*-:")

dirName :: ParsecT String u Identity Char
dirName = noneOf "#*-:"

fullName :: ParsecT String u Identity String
fullName = (:) <$> dirName <*> (char '.' >> fileName)

i32 :: Integral a => a -> Word32
i32 = fromIntegral

parseMetaData :: ParsecT String u Identity (String, MetaData)
parseMetaData = do
    f <- fullName
    load <- spaces >> (hexadecimal <* spaces)
    exec <- hexadecimal <* many (char ' ') <* endOfLine
    return (f, MetaData (i32 load) (i32 exec))

parseCatalogue :: ParsecT String u Identity MetaCatalogue
parseCatalogue = M.fromList <$> many parseMetaData

readMetaCatalogue :: String -> IO MetaCatalogue
readMetaCatalogue name = do
    h' <- tryIOError $ openFile name ReadMode
    case h' of
        Left _ -> return M.empty
        Right h -> do
            liftIO $ putStrLn $ "hGetContents " ++ name
            d' <- B.hGetContents h
            let d = map BS.w2c $ B.unpack d'
            let mc = parse parseCatalogue "" d
            hClose h
            return $ either (const M.empty) id mc

writeMetaCatalogue :: String -> MetaCatalogue -> IO ()
writeMetaCatalogue name ms = do
    h <- openFile name WriteMode
    forM_ (M.toList ms) $ \(filename, MetaData load exec) -> do
        hPrintf h "%c.% -7s %08x %08x\n"
                  (head filename) (tail filename) load exec
    hClose h

compactName :: String -> String
compactName (c : '.' : cs) = c : cs

setMetaData :: String -> Word32 -> Word32 -> IO ()
setMetaData filename load exec = do
    mv <- readMetaCatalogue ".BBC_Store"
    let mv' = M.insert (compactName filename) (MetaData load exec) mv
    writeMetaCatalogue ".BBC_Store" mv'

getMetaData :: String -> IO (Word32, Word32)
getMetaData filename = do
    mv <- readMetaCatalogue ".BBC_Store"
    case M.lookup (compactName filename) mv of
        Nothing -> return (0, 0)
        Just (MetaData load exec) -> return (load, exec)

deleteMetaData :: String -> IO ()
deleteMetaData filename = do
    mv <- readMetaCatalogue ".BBC_Store"
    let mv' = M.delete filename mv
    writeMetaCatalogue ".BBC_Store" mv'

main :: IO ()
main = do
--    mv <- readMetaCatalogue ".BBC_Store"
--    let mv' = mv & catalogue . ix "WWELCOME" . loadAddress +~ 1
--    writeMetaCatalogue ".BBC_Store" mv'
    setMetaData "WWELCOME" 0x0 0x1900
    setMetaData "!BOOT" 0xffffffff 0x0e01
    setMetaData "$SKETCH" 0x0000 0x0900
    setMetaData "XFRED" 0x0000 0x0a00
    u <- getMetaData "WWELCOME"
    print u
    deleteMetaData "$SKETCH"
