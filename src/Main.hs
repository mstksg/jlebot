{-# LANGUAGE Arrows #-}

module Main where

import Auto
import Prelude hiding (mapM_)
import Data.List
import Control.Applicative
import Control.Arrow
import Control.Monad.IO.Class
import Data.Foldable

type Interact m = Auto m String (Maybe String)

main :: IO ()
main = loopAuto myAuto

loopAuto :: MonadIO m => Interact m -> m ()
loopAuto a' = do
    inp      <- liftIO (putStr "> " >> getLine)
    (out, a) <- stepAuto a' inp
    liftIO    $ mapM_ putStrLn out
    loopAuto a

seqAuto :: Monad m => [Interact m] -> Interact m
seqAuto []     = pure Nothing
seqAuto (a:as) = proc inp -> do
    out <- a -< inp
    case out of
      Just _ ->
        returnA    -< out
      Nothing   ->
        seqAuto as -< inp

myAuto :: Monad m => Interact m
myAuto = seqAuto [countAuto]

countAuto :: Monad m => Interact m
countAuto = proc inp ->
    if "c" `isPrefixOf` inp
      then returnA -< Just "hey"
      else returnA -< Nothing
