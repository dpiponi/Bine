module MonadInput where

import System.Console.Haskeline

class MonadInput m where
    inputLine :: String -> m (Maybe String)

instance MonadException m => MonadInput (InputT m) where
    inputLine = getInputLine
