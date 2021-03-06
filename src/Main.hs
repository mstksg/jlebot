{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE TupleSections #-}

module Main where

-- import Control.Applicative
-- import Data.Traversable
-- import System.Directory
import Auto
import Backend.IRC
import Backend.StdIn
import Control.Arrow
import Control.Monad hiding      (mapM_, forM_)
import Control.Monad.IO.Class
import Data.Foldable
import Data.Monoid
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

myAuto :: MonadIO m => Interact m
myAuto = mconcat [ saveIt "count"   $ i' countAuto
                 , i' pollAuto
                 , saveIt "greet"   $ i' greetAuto
                 , saveIt "karma"   $ i' karmaAuto
                 -- , i' haskAuto
                 , lambdabotAuto
                 , i' reconAuto
                 , i' askAuto
                 , i' censorAuto
                 , i' pokeAuto
                 , i' dogeAuto
                 , i' helpAuto
                 , i' keywordAuto
                 , saveIt "hangman" $ hangmanAuto
                 , saveIt "crypto"  $ cryptogramAuto
                 , mouthAuto
                 , saveIt "markov"  $ i' markovAuto
                 ]

saveIt :: MonadIO m => FilePath -> Auto m a b -> Auto m a b
saveIt fp = savingAuto ("data/saving/" ++ fp ++ ".dat")
-- saveIt fp = id

i' :: Monad m => Interact' m -> Interact m
i' a0 = proc msg@(InMessage _ _ o _) -> do
            res <- a0 -< msg
            returnA -< OutMessages (M.singleton o res)
