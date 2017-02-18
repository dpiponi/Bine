{-# LANGUAGE FlexibleContexts #-}

module TraceLog where

import State6502
import Core
import System.IO
import Monad6502
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Lens

tracelog :: (Emu6502 m, MonadState State6502 m) => String -> m ()
tracelog line = do
    handle <- use logFile
    case handle of
        Nothing -> return ()
        Just h -> do
            liftIO $ hPutStrLn h line
            liftIO $ hFlush h
