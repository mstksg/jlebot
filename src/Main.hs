{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE TupleSections #-}

module Main where

-- import Control.Applicative
-- import Data.Monoid
-- import System.Directory
import Backend.IRC
import Backend.StdIn
import Control.Arrow
import Control.Monad hiding      (mapM_, forM_)
import Control.Monad.IO.Class
import Data.Foldable
import Data.Traversable
import Module
import Prelude hiding            (mapM_, sequence, foldr, concat, elem)
import System.Environment
import Types
import qualified Data.Map.Strict as M

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
autoModules = fmap combineOutMessages . sequenceA

myAuto :: MonadIO m => Interact m
myAuto = autoModules [ i' countAuto
                     , i' pollAuto
                     , i' greetAuto
                     , i' karmaAuto
                     , i' haskAuto
                     , i' reconAuto
                     , i' askAuto
                     , i' censorAuto
                     , i' pokeAuto
                     , mouthAuto
                     ]

i' :: Monad m => Interact' m -> Interact m
i' a0 = proc msg@(InMessage _ _ o _) -> do
            res <- a0 -< msg
            returnA -< OutMessages (M.singleton o res)
