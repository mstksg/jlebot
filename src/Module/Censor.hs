{-# LANGUAGE OverloadedStrings #-}

module Module.Censor where

import Types
import Data.Char
import Control.Arrow
import qualified Data.Text as T
import Data.Text (Text)
import Control.Monad
import System.Random
import Data.Time
import Data.Time.Clock.POSIX
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

censorAuto :: Monad m => Interact' m
censorAuto = arr censor

censor :: InMessage -> [String]
censor (InMessage n msg _ t)
    | textMsg /= censorAll && choice = return $  n
                                              ++ " meant to say: "
                                              ++ T.unpack censorAll
    | otherwise                      = mzero
  where
    textMsg   = T.pack msg
    censorAll = foldl' (\s (a,b) -> T.replace a b s) textMsg reps
    gen  = mkStdGen
         . (+ sum (map ord (n ++ msg)))
         . round
         . (* 1000)
         . utctDayTime
         $ t
    choice = (== (1 :: Int)) . fst . randomR (1,16) $ gen


reps :: [(Text, Text)]
reps = [ ("fuck"   , "intercourse")
       , ("shit"   , "poop")
       , ("bitch"  , "nice lady")
       , ("slut"   , "friendly lady")
       , ("shekels", "dat paper")
       , ("wow"    , "WOW")
       , ("hell"  , "florida")
       , ("fack"   , "intarcourse")
       ]
