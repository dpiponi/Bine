module FileSystems where

import System.IO
import Control.Monad.IO.Class
import Control.Monad
import qualified Data.ByteString as B
import qualified Data.Map as M
import Data.Word
import qualified Data.ByteString.Internal as BS (c2w, w2c)
import Data.Bits
import Numeric

data VHandle = HHandle Handle | BHandle Int B.ByteString

-- load. execute.
data BBCFile = BBCFile Word32 Word32 B.ByteString
