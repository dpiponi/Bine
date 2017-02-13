module FileSystems where

import System.IO
import qualified Data.ByteString as B
import qualified Data.Map as M
import Data.Word

data VHandle = HHandle Handle | BHandle Int B.ByteString

-- load. execute.
data FileSystem = Host | Bytes Word32 Word32 (M.Map String B.ByteString)
