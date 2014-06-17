module Module.Help (helpAuto) where

import Types
import Control.Monad
import Data.Monoid
import Control.Arrow

helpAuto :: Monad m => Interact' m
helpAuto = arr (helpPure . inMessageBody)

helpPure :: String -> [String]
helpPure str = case words str of
                 "@help":_ ->
                     return $ "I am jlebot!  "
                           <> "Written in Haskell, maintained by jle`.  "
                           <> "https://github.com/mstksg/jlebot"
                 _         ->
                     mzero
