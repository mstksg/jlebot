{-# LANGUAGE Arrows #-}


module Module.Doge (dogeAuto) where

import Types
import Data.Maybe
import Data.Digest.Pure.MD5
import Data.Monoid
import Control.Monad.Random
import Control.Monad
import Control.Category
import Prelude hiding ((.))
import Data.Char
import Control.Applicative
import Data.Binary
import Auto
import Control.Arrow
import qualified Data.ByteString.Lazy as B
import Control.Monad.IO.Class

loadWords :: IO [String]
loadWords = (map . map) toLower . lines <$> readFile "/usr/share/dict/american-english"

dogeAuto :: MonadIO m => Interact' m
dogeAuto = proc im -> do
    dogeFunc <- arr doge . cacheAuto (liftIO loadWords)    -< ()
    returnA -< dogeFunc im

doge :: [String] -> InMessage -> [String]
doge pool (InMessage _ b _ t) =
    case words b of
      "@doge":rest -> let rest' = (map . map) toLower rest
                          rest'' | null rest' = [show t]
                                 | otherwise  = rest'
                          dig   = md5 (encode rest'')
                          gen   = mkStdGen    . fromIntegral
                                . fromMaybe 0 . listToMaybe
                                . B.unpack    . encode
                                $ dig
                      in  evalRand (randoge pool rest) gen
      _            -> mzero

randoge :: [String] -> [String] -> Rand StdGen [String]
randoge pool wants = ((++ ["wow"]).) . zipWith (<>)
                 <$> replicateM 3 (selectRand prefs)
                 <*> fromPool
  where
    prefs    = map (++ " ") prefixes
    fromPool = (wants ++) <$> replicateM 3 (selectRand pool)

prefixes :: [String]
prefixes = [ "such"
           , "wow"
           , "much"
           , "very"
           , "great"
           , "so"
           , "awesome"
           , "much"
           , "such"
           , "very"
           ]

selectRand :: RandomGen g => [a] -> Rand g a
selectRand pool = (pool !!) <$> getRandomR (0, length pool - 1)

