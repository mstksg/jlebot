module Module.Keyword (keywordAuto) where

import Types
import Control.Monad
import Control.Arrow
import Data.Maybe
import Prelude hiding (lookup)
import Data.Map.Strict (Map, fromList, lookup)
-- import qualified Data.Map.Strict as M

keywordAuto :: Monad m => Interact' m
keywordAuto = arr (fromMaybe mzero . keyword . inMessageBody)

keyword :: String -> Maybe [String]
keyword str = flip lookup keywords =<< listToMaybe (words str)


keywords :: Map String [String]
keywords = fromList [ ("@flipJ", ["(╯°□°）╯︵ ┻━┻"])
                    , ("@flip", ["(J °O°)J JL_JL"])
                    ]
