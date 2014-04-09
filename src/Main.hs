{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Main where

-- import System.Directory
import Backend.IRC
import Backend.StdIn
import Control.Applicative
import Control.Monad hiding   (mapM_, forM_)
import Control.Monad.IO.Class
import Data.Foldable
import Data.Traversable
import Module
import Prelude hiding         (mapM_, sequence, foldr, concat, elem)
import System.Environment
import Types

stateFile :: FilePath
stateFile = "data/state"

main :: IO ()
main = do
    modes <- getArgs
    -- when ("reset" `elem` modes) (removeFile stateFile)
    if "irc" `elem` modes
      then ircLoop "data/state_irc" myAuto
      else stdinLoop "data/state_stdin" myAuto

autoModules :: Monad m => [Interact m] -> Interact m
autoModules = fmap (foldr (<|>) mzero) . sequenceA

myAuto :: MonadIO m => Interact m
myAuto = autoModules [countAuto, pollAuto, greetAuto, karmaAuto]
