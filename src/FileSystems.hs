module FileSystems where

import System.IO
import qualified Data.ByteString as B
import qualified Data.Map as M

data VHandle = HHandle Handle | BHandle Int B.ByteString

data FileSystem = Host | Bytes (M.Map String B.ByteString)
