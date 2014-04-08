{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Backend.StdIn
import Control.Applicative
import Control.Monad hiding   (mapM_, forM_)
import Control.Monad.IO.Class
import Data.Foldable
import Data.Traversable
import Module
import Prelude hiding         (mapM_, sequence, foldr, concat)
import Types

main :: IO ()
-- main = ircLoop "data/state" myAuto
main = stdinLoop "data/state" myAuto

autoModules :: Monad m => [Interact m] -> Interact m
autoModules = fmap (foldr (<|>) mzero) . sequenceA

myAuto :: MonadIO m => Interact m
myAuto = autoModules [countAuto, pollAuto, greetAuto]
