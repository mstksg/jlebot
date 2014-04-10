{-# LANGUAGE Arrows #-}

module Module.Karma (karmaAuto) where

import Auto
import Control.Arrow
import Control.Monad
import Data.List
import Data.Map.Strict           (Map)
import Data.Maybe
import Types
import qualified Data.Map.Strict as M

karmaAuto :: Monad m => Interact m
karmaAuto = proc (InMessage _ msg) -> do
    let pos      = M.fromList $ zip (karms "++" msg) (repeat 1)
        neg      = M.fromList $ zip (karms "--" msg) (repeat (-1))
        newkarm  = M.unionWith (+) pos neg :: Map String Int
    karmalist <- scanA (M.unionWith (+)) M.empty -< newkarm
    returnA -< case words msg of
      "@karma":n:_ -> return $ n ++ ": " ++ show (M.findWithDefault 0 n karmalist)
      _            -> mzero


karm :: String -> String -> Maybe String
karm pre str | pre `isSuffixOf` str =   Just
                                      . reverse
                                      . drop (length pre)
                                      . reverse
                                      $ str
             | otherwise            = Nothing

karms :: String -> String -> [String]
karms pre = mapMaybe (karm pre) . words
