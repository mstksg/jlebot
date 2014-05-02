{-# LANGUAGE Arrows #-}

module Module.Proxy (proxyAuto, proxyAllAuto) where

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


-- why would you want this?
proxyAllAuto :: Monad m => String -> Interact m
proxyAllAuto = flip proxyAuto return

proxyAuto :: Monad m => String -> (String -> Maybe String) -> Interact m
proxyAuto dest filt = proc imsg -> do
    askEvt' <- whenE ((/= dest) . inMessageSource) -< imsg
    resEvt  <- whenE ((== dest) . inMessageSource) -< imsg

    let askMsg = filt . inMessageBody =<< askEvt'
        askEvt = imsg                   <$ askMsg
        resMsg = inMessageBody          <$> resEvt
        qAdd   = QAdd . inMessageSource <$> askEvt
        qPop   = QPop                   <$  resEvt

    chanOut <- queueAuto -< qPop <|> qAdd

    let chanOutMap = fromMaybe mempty $ do
                       c  <- chanOut
                       rm <- resMsg
                       return (M.singleton c [rm])
        lbOutMap   = fromMaybe mempty
                   $ M.singleton dest . return <$> askMsg

    returnA -< foldMap OutMessages [chanOutMap, lbOutMap]

queueAuto :: Monad m => Auto m (Event QueueIn) (Event String)
queueAuto = join <$> discrete (stateAuto updateQueue mempty)
  where
    updateQueue (QAdd chan) q = (noEvent, q |> chan)
    updateQueue QPop        q = case viewl q of
                                  x :< q' -> (event x, q')
                                  _       -> (noEvent, q )
