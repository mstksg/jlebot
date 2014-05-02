{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Backend.IRC where

import Auto
import Control.Applicative
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
              { cChannels = ["#jlebot-test","#ucsd","#jlebot-abuss"]
              , cEvents   = [Privmsg (onMessage a)]
              }

launchIRC :: MVar (Interact IO) -> IO ()
launchIRC amvr = () <$ connect (ircConf amvr) False True

-- launchIRC :: MVar (Interact IO) -> IO ()
-- launchIRC amvr = do
--     let conf = ircConf amvr
--     conn <- connect conf False True
--     forM_ conn $ \mirc -> do
--       forM_ (cChannels conf) $ \chn -> do
--         sendMsg mirc (C8.pack chn) "hello everyone!"

ircLoop :: FilePath -> Interact IO -> IO ()
ircLoop fp a0 = do
    amvr <- newMVar =<< loadAutoFile fp a0
    catch (launchIRC amvr) $ \(_ :: AsyncException) ->
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

