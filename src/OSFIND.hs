{-# LANGUAGE FlexibleContexts #-}

module OSFIND where

import Control.Monad.State
import System.IO
import Utils
import Control.Lens
import State6502
import Monad6502
import Utils
import Core
import qualified Data.IntMap as M

freeHandle :: M.IntMap Handle -> Int
freeHandle hs =
    let freeHandle' i = if i `M.notMember` hs then i else freeHandle' (i+1)
    in freeHandle' 1

{-# INLINABLE osfind #-}
osfind :: (MonadState State6502 m, Emu6502 m) => m ()
osfind = do
    a <- getA
    x <- getX
    y <- getY
    case a of
        -- Close all files
        0x00 -> do
            hs <- use handles
            when (y == 0) $ forM_ hs $ liftIO . hClose
            handles .= M.empty
            let k = fromIntegral y
            let mh = M.lookup k hs
            case mh of
                Nothing -> error $ "Unknown handle #" ++ show k
                Just h -> do
                    liftIO $ putStrLn $ "Closing #" ++ show k
                    liftIO $ hClose h
                    handles %= M.delete k

        0x80 -> do
            let addr = make16 x y
            filename <- stringAt addr
            h <- liftIO $ openFile filename WriteMode
            hs <- use handles
            let k = freeHandle hs
            handles %= M.insert k h
            liftIO $ putStrLn $ "Opened " ++ filename ++ " with handle " ++ show k
            putA (i8 k)

        _ -> liftIO $ putStrLn $ "Unknown OSFIND call: " ++ show a ++ "," ++ show x ++ "," ++ show y
