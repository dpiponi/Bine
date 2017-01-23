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
import Text.Parsec
import OSBYTE

data Command = FX Int Int Int deriving Show

decimal :: ParsecT String u Identity Int
decimal = do
    digits <- many1 digit
    return $ read digits

parseCommand :: ParsecT String u Identity Command
parseCommand = FX <$> do
                            (string "fx" >> spaces >> decimal)
                            <*> option 0 (spaces >> char ',' >> spaces >> decimal)
                            <*> option 0 (spaces >> char ',' >> spaces >> decimal)

execStarCommand :: (Emu6502 m, MonadState State6502 m) => Command -> m ()
execStarCommand (FX a x y) = osbyte (i8 a) (i8 x) (i8 y)

{-# INLINABLE oscli #-}
oscli :: (MonadState State6502 m, Emu6502 m) => m ()
oscli = do
    lo <- getX
    hi <- getY
    let addr = make16 lo hi
    cmd <- stringAt addr
    let cmd' = removeStars cmd
    liftIO $ putStrLn cmd'
    let cmd'' = parse parseCommand "" cmd'
    case cmd'' of
        Right cmd''' -> execStarCommand cmd'''
        Left _ -> void $ liftIO $ system cmd'
    return ()
