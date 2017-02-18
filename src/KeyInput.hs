--
-- Manage keyboard.
-- Includes things like characters inserted into keyboard buffer
-- using *FX138 and, eventually, *EXEC.
--
{-# LANGUAGE FlexibleContexts #-}

module KeyInput where

import Deque
import Data.Word
import Control.Lens hiding (unsnoc, cons)
import Data.Foldable
import qualified Data.ByteString.Internal as BS
import Data.Array
import Core
import Control.Monad.State
import State6502
import Monad6502
import Control.Monad.IO.Class

-- There are probably more sequences needed like maybe |!|? = 255
escape :: [Word8] -> [Word8]
escape (124 : 63 : ks) = 127 : escape ks -- |?
escape (124 : 124 : ks) = 124 : escape ks -- ||
escape (124 : 33 : 124 : k : ks) | k >=64+1 && k <= 64+26 = k-64+128 : escape ks -- |!|A
escape (124 : 33 : k : ks) = k+128 : escape ks -- |!A
escape (124 : k : ks) | k >=64+1 && k <= 64+26 = k-64 : escape ks -- |A
escape (k : ks) = k : escape ks
escape [] = []

getKey :: (Emu6502 m, MonadState State6502 m) => m Word8
getKey = do
    queue <- use keyQueue
    case unsnoc (buffer queue) of
        Nothing -> do
            c <- liftIO getChar
            return (BS.c2w c)
        Just (k, ks) -> do
            keyQueue .= KeyInput ks (keydefs queue) -- XXX lens
            return k

insertKey :: MonadState State6502 m => Word8 -> m ()
insertKey k | k >= 128 && k < 138 = do
    KeyInput ks defs <- use keyQueue
    keyQueue .= KeyInput (foldr cons ks (defs!(fromIntegral k-128))) defs
insertKey k = do
    KeyInput ks defs <- use keyQueue
    keyQueue .= KeyInput (k `cons` ks) defs -- XX lens

defineKey :: (MonadState State6502 m) => Int -> [Word8] -> m ()
defineKey k def = do
    KeyInput buffer defs <- use keyQueue
    keyQueue .= KeyInput buffer (defs // [(k, escape def)])

removeKeys :: MonadState State6502 m => m ()
removeKeys = do
    KeyInput _ defs <- use keyQueue
    keyQueue .= KeyInput (fromList []) defs

allKeys :: MonadState State6502 m => m [Word8]
allKeys = do
    queue <- use keyQueue
    removeKeys
    return (toList (buffer queue))

putKeys :: MonadState State6502 m => [Word8] -> m ()
putKeys ks = do
    KeyInput buffer defs <- use keyQueue
    keyQueue .= KeyInput (foldr cons buffer ks) defs

emptyQueue :: KeyInput
emptyQueue = KeyInput (fromList []) (listArray (0, 9) [[] | i <- [0..9]])
