module FileSystems where

import System.IO
import qualified Data.ByteString as B

data VHandle = HHandle Handle | BHandle Int B.ByteString
