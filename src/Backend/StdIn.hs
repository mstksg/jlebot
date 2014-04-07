module Backend.StdIn where

import Types
import Auto
import Control.Monad.IO.Class
import System.IO
import Control.Exception

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
        (out, a) <- stepAuto a' (InMessage "justin" inp')
        liftIO    $ mapM_ putStrLn out
        loopAuto a
      Left _ -> return a'


