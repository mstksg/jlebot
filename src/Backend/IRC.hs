{-# LANGUAGE ScopedTypeVariables #-}

module Backend.IRC where

import Auto
import Control.Concurrent
import Control.Exception
import Control.Monad                   (void, mzero)
import Data.Foldable
import Network.SimpleIRC
import Prelude hiding                  (mapM_)
import Types
import qualified Data.ByteString.Char8 as C8

ircConf :: MVar (Interact IO) -> IrcConfig
ircConf a = (mkDefaultConfig "irc.freenode.org" "jlebot")
              { cChannels = ["#jlebot-test","#ucsd"]
              , cEvents   = [Privmsg (onMessage a)]
              }

ircLoop :: FilePath -> Interact IO -> IO ()
ircLoop fp a0 = do
    amvr <- newMVar =<< loadAutoFile fp a0
    catch (void $ connect (ircConf amvr) False True) $ \(_ :: AsyncException) ->
      withMVar amvr (writeAutoFile "data/state")
    return ()

onMessage :: MVar (Interact IO) -> EventFunc
onMessage amvr server msg = do
    resp <- modifyMVar amvr $ \a' ->
      case mNick msg of
        Just nick -> do
          (out,a) <- stepAuto a' (InMessage (C8.unpack nick) (C8.unpack (mMsg msg)))
          return (a,out)
        Nothing   ->
          return (a',mzero)
    forM_ (mOrigin msg) $ \o ->
      mapM_ (sendMsg server o . C8.pack) resp

