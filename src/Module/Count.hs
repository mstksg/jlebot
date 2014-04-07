{-# LANGUAGE Arrows #-}

module Module.Count where

import Auto
import Control.Arrow
import Data.List
import Control.Monad
import Types

countAuto :: Monad m => Interact m
countAuto = proc (InMessage _ inp) ->
    if "@c" `isPrefixOf` inp
      then do
        c <- integral 0 -< 1 :: Int
        returnA -< return (show c)
      else
        returnA -< mzero

