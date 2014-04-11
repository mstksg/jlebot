{-# LANGUAGE ScopedTypeVariables #-}

module Backend.IRC where

import Auto
import Data.Monoid
import Control.Concurrent
import Control.Exception
import Control.Monad                   (void)
import Data.Foldable
import Data.Time
import Network.SimpleIRC
import Prelude hiding                  (mapM_)
import Types
import qualified Data.ByteString.Char8 as C8
import qualified Data.Map.Strict       as M

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
    OutMessages resps <- modifyMVar amvr $ \a' ->
      case (mNick msg, mOrigin msg) of
        (Just nick, Just orig) -> do
          t <- getCurrentTime
          (out,a) <- stepAuto a' (InMessage (C8.unpack nick) (C8.unpack (mMsg msg)) (C8.unpack orig) t)
          return (a,out)
        _   ->
          return (a',mempty)

    void . flip M.traverseWithKey resps $ \k v ->
      mapM_ (sendMsg server (C8.pack k) . C8.pack) v

