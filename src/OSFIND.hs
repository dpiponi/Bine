{-# LANGUAGE FlexibleContexts #-}

module OSFIND where

import Control.Monad.State
import System.IO
import Utils
import Control.Lens
import State6502
import FileSystems
import Monad6502
import Utils
import Core
import qualified Data.IntMap as M

freeHandle :: M.IntMap VHandle -> Int
freeHandle hs =
    let freeHandle' i = if i `M.notMember` hs then i else freeHandle' (i+1)
    in freeHandle' 1

closeFile :: VHandle -> IO ()
closeFile (HHandle h) = hClose h
closeFile (BHandle _ _) = return ()

closeAllFiles :: M.IntMap VHandle -> IO ()
closeAllFiles hs = forM_ hs closeFile

-- XXX Update to use hostName

{-# INLINABLE osfind #-}
osfind :: (MonadState State6502 m, Emu6502 m) => m ()
osfind = do
    a <- getA
    x <- getX
    y <- getY
    case a of
        -- Causes a file or files to be closed.
        0x00 -> do
            hs <- use handles
            if y == 0 
                then do 
                    liftIO (closeAllFiles hs)
                    handles .= M.empty
                else do
                    let k = fromIntegral y
                    let mh = M.lookup k hs
                    case mh of
                        Nothing -> error $ "Unknown handle #" ++ show k
                        Just h -> do
                            liftIO $ putStrLn $ "Closing #" ++ show k
                            liftIO $ closeFile h
                            handles %= M.delete k

        -- Causes a file to be opened for output (writing).
        -- For now defaulting to "Host" OS
        0x80 -> do
            let addr = make16 x y
            filename <- stringAt addr
            h <- liftIO $ openFile filename WriteMode
            hs <- use handles
            let k = freeHandle hs
            handles %= M.insert k (HHandle h)
            liftIO $ putStrLn $ "Opened " ++ filename ++ " with handle " ++ show k
            putA (i8 k)

        _ -> liftIO $ putStrLn $ "Unknown OSFIND call: " ++ show a ++ "," ++ show x ++ "," ++ show y
