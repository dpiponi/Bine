{-# LANGUAGE FlexibleContexts #-}

module OSCLI where

import Control.Monad.State
import System.IO
import System.Process
import Utils
import MonadInput
import Control.Lens
import State6502
import Monad6502
import Utils
import Core
import qualified Data.IntMap as M
import qualified Data.ByteString.Internal as BS (c2w, w2c)
import Data.Word
import Numeric
import Data.Bits

{-# INLINABLE oscli #-}
oscli :: (MonadState State6502 m, Emu6502 m) => m ()
oscli = do
    lo <- getX
    hi <- getY
    let addr = make16 lo hi
    {-
    let loop cmd i = do
                    byte <- readMemory (addr+i16 i)
                    if byte /= 0x0d
                        then loop (cmd ++ [BS.w2c byte]) (i+1)
                        else return cmd
    cmd <- loop "" 0
    -}
    cmd <- stringAt addr
    let cmd' = removeStars cmd
    liftIO $ putStrLn cmd'
    liftIO $ system cmd'
    return ()
