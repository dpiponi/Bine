module FileSystems where

import System.IO
import qualified Data.ByteString as B
import qualified Data.Map as M
import Data.Word
import qualified Data.ByteString.Internal as BS (c2w, w2c)

data VHandle = HHandle Handle | BHandle Int B.ByteString

-- load. execute.
data FileSystem = Host | Bytes Word32 Word32 (M.Map String B.ByteString)

getPadded :: B.ByteString -> Int -> Int -> String
getPadded image 0 start = ""
getPadded image n start =
    let p = image `index` start
    in if p == 0 || p == 32
          then ""
          else BS.w2c p : getPadded image (n-1) (start+1)

getVolumeName :: B.ByteString -> String
getVolumeName image = getPadded image 8 0

-- http://beebwiki.mdfs.net/Acorn_DFS_disc_format
loadImage :: String -> IO FileSystem
loadImage filename = do
    handle <- openFile filename ReadMode
    image <- B.hGetContents handle

    volumeName <- getVolumeName image 

    hClose handle

    return Host
