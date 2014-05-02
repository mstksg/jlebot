module Module.Lambdabot (lambdabotAuto) where

import Module.Proxy
import Types

getCommand :: String -> Maybe String
getCommand str = case words str of
                   ">>":rest  -> Just (unwords (">":rest))
                   ":t":_     -> Just str
                   "@pl":_    -> Just str
                   "@djinn":_ -> Just str
                   _          -> Nothing


lambdabotAuto :: Monad m => Interact m
lambdabotAuto = proxyAuto "lambdabot" getCommand
