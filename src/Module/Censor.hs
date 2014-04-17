{-# LANGUAGE OverloadedStrings #-}

module Module.Censor where

import Types
import Control.Arrow
import qualified Data.Text as T
import Data.Text (Text)
import Control.Monad
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

censorAuto :: Monad m => Interact' m
censorAuto = arr censor

censor :: InMessage -> [String]
censor (InMessage n msg _ _) | textMsg /= censorAll = [  n
                                                      ++ " meant to say: "
                                                      ++ T.unpack censorAll
                                                      ]
                             | otherwise            = mzero
-- censor (InMessage n msg _ _) | any fst censorPairs = [  n
--                                                      ++ " meant to say: "
--                                                      ++ unwords (map snd censorPairs)
--                                                      ]
--                              | otherwise           = mzero
  where
    textMsg   = T.pack msg
    censorAll = foldl' (\s (a,b) -> T.replace a b s) textMsg reps
    -- censorPairs = map censorWord (words msg)
    -- censorWord word = case word `M.lookup` reps of
    --                     Just rep -> (True, rep)
    --                     Nothing  -> (False, word)
    -- | any (`isInfixOf` msg) (M.elems reps) = [n ++ " meant to say: " ++ M.findWithDefault "" ]
    -- | otherwise = mzero

reps :: [(Text, Text)]
reps = [ ("fuck"   , "intercourse")
       , ("shit"   , "poop")
       , ("cunt"   , "nice lady")
       , ("bitch"  , "nice lady")
       , ("slut"   , "friendly lady")
       , ("shekels", "dat paper")
       , ("wow"    , "WOW")
       , ("hell"   , "florida")
       , ("fack"   , "intarcourse")
       ]
