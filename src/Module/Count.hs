{-# LANGUAGE Arrows #-}

module Module.Count where

import Auto
import Control.Arrow
import Control.Monad
import Data.List
import Types

countAuto :: Monad m => Interact' m
countAuto = proc (InMessage _ inp _ _) ->
    if "@c" `isPrefixOf` inp
      then do
        c <- integral 0 -< 1 :: Int
        returnA -< return (show c)
      else
        returnA -< mzero

