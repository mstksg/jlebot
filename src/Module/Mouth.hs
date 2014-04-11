module Module.Mouth where

import Control.Arrow
import Data.Monoid
import Types
import qualified Data.Map.Strict as M

mouthAuto :: Monad m => Interact m
mouthAuto = arr mouth

mouth :: InMessage -> OutMessages
mouth (InMessage "jle`" str _ _) = case words str of
                                     "ANN":to:rest -> OutMessages $ M.singleton to [unwords rest]
                                     _             -> mempty
mouth _                          = mempty

