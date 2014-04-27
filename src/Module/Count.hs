{-# LANGUAGE Arrows #-}

module Module.Count where

import Auto
import Control.Arrow
import Control.Monad
-- import Data.List
import Types
-- import Data.Maybe

countAuto :: Monad m => Interact' m
countAuto = proc (InMessage _ inp _ _) ->
    case words inp of
      "@c":_ -> do
        c <- integral 0 -< 1 :: Int
        returnA -< return (show c)
      _ ->
        returnA -< mzero

