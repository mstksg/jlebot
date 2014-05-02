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

karmaAuto :: Monad m => Interact' m
karmaAuto = proc (InMessage nick msg _ _) -> do
    let pos      = M.fromList (zip (karms "++" msg) (repeat 1))
        pos'     = M.filterWithKey (\k _ -> k /= nick) pos
        neg      = M.fromList (zip (karms "--" msg) (repeat (-1)))
        newkarm  = M.unionWith (+) pos' neg

    karmalist <- scanA (M.unionWith (+)) M.empty -< newkarm

    returnA -< maybeToList (showKarma msg karmalist)

karm :: String -> String -> Maybe String
karm pre str | pre `isSuffixOf` str =   Just
                                      . reverse
                                      . drop (length pre)
                                      . reverse
                                      $ str
             | otherwise            = Nothing

karms :: String -> String -> [String]
karms pre = mapMaybe (karm pre) . words

showKarma :: String -> Map String Int -> Maybe String
showKarma msg m = case words msg of
                    "@karma":n:_ -> return $ n
                                          ++ ": "
                                          ++ show (M.findWithDefault 0 n m)
                    _            -> mzero
