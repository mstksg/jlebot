module Backend.StdIn where

import Auto
import Control.Exception
import Control.Monad.IO.Class
import Data.Foldable
import Data.Time
import Prelude hiding         (mapM_)
import System.IO
import Types

stdinLoop :: MonadIO m => FilePath -> Interact m -> m ()
stdinLoop fp a0 = do
    a  <- liftIO $ loadAutoFile fp a0
    a' <- loopAuto a
    liftIO $ writeAutoFile "data/state" a'

loopAuto :: MonadIO m => Interact m -> m (Interact m)
loopAuto a' = do
    inp      <- liftIO $ do
                  putStr "> "
                  hFlush stdout
                  try getLine :: IO (Either SomeException String)
    case inp of
      Right inp' -> do
        t        <- liftIO getCurrentTime
        (out, a) <- stepAuto a' (InMessage "justin" inp' "" t)
        liftIO    $ (mapM_ . mapM_) putStrLn (outMessageMap out)
        loopAuto a
      Left _ -> return a'


