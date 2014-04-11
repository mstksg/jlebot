module Module.Recon where

import Control.Arrow
import Control.Monad
import Types

reconAuto :: Monad m => Interact m
reconAuto = arr (recon . inMessageBody)

recon :: String -> [String]
recon str = case words str of
              "@recon":n:_ -> [ "Looking up information on " ++ n
                              , "..."
                              , n ++ ": Major -- Computer Science and Engineering"
                              , "Data exhausted"
                              ]
              _            -> mzero
