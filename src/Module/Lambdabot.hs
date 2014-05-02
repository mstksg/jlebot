{-# LANGUAGE Arrows #-}

module Module.Lambdabot (lambdabotAuto) where

-- import Data.Binary
-- import Data.Traversable
-- import GHC.Generics
import Auto
import Control.Applicative
import Control.Arrow
import Control.Category
import Control.Monad hiding      (sequence)
import Data.Foldable
import Data.Maybe
import Data.Monoid
import Data.Sequence
import Event
import Prelude hiding            ((.), sequence)
import Types
import qualified Data.Map.Strict as M

data QueueIn = QAdd String
             | QPop


getCommand :: String -> Maybe String
getCommand str = case words str of
                   ">>":rest  -> Just (unwords (">":rest))
                   ":t":_     -> Just str
                   "@pl":_    -> Just str
                   "@djinn":_ -> Just str
                   _          -> Nothing


lambdabotAuto :: Monad m => Interact m
lambdabotAuto = proc imsg -> do
    askEvt' <- whenE ((/= "lambdabot") . inMessageSource) -< imsg
    resEvt  <- whenE ((== "lambdabot") . inMessageSource) -< imsg

    let askMsg = getCommand . inMessageBody =<< askEvt'
        askEvt = imsg <$ askMsg
        resMsg = inMessageBody <$> resEvt
        qAdd   = QAdd . inMessageSource <$> askEvt
        qPop   = QPop                   <$  resEvt

    chanOut <- queueAuto -< qPop <|> qAdd

    let chanOutMap = fromMaybe mempty $ do
                       c  <- chanOut
                       rm <- resMsg
                       return (M.singleton c [rm])
        lbOutMap   = fromMaybe mempty
                   $ M.singleton "lambdabot" . return
                 <$> askMsg

    returnA -< foldMap OutMessages [chanOutMap, lbOutMap]

queueAuto :: Monad m => Auto m (Event QueueIn) (Event String)
queueAuto = join <$> discrete (stateAuto updateQueue mempty)
  where
    updateQueue (QAdd chan) q = (noEvent, q |> chan)
    updateQueue QPop        q = case viewl q of
                                  x :< q' -> (event x, q')
                                  _       -> (noEvent, q )
