{-# LANGUAGE Arrows #-}

module Main where

import Auto
import Control.Applicative
import Control.Arrow
import Control.Exception
import Network
import Control.Monad hiding           (mapM_)
import Control.Monad.IO.Class
import Data.Foldable
import Data.List hiding               (foldr, concat)
import Data.Traversable
import Prelude hiding                 (mapM_, sequence, foldr, concat)
import System.IO
import qualified Data.ByteString.Lazy as B

type Interact m = Auto m String [String]

main :: IO ()
main = stdinLoop

ircLoop :: IO ()
ircLoop = do
    h <- connectTo "irc.freenode.org" (PortNumber 6667)
    hSetBuffering h NoBuffering
    write h "NICK" "jlebot"
    write h "USER" "jlebot 0 * :jle bot"
    write h "JOIN" "#jlebot-test"
    listen h

write :: Handle -> String -> String -> IO ()
write h s t = do
    hPutStrLn h $ s ++ " " ++ t ++ "\r"
    putStrLn    $ "> " ++ s ++ " " ++ t

listen :: Handle -> IO ()
listen h = forever $ do
    s <- hGetLine h
    putStrLn s


stdinLoop :: IO ()
stdinLoop = do
    h <- try (openFile "data/state" ReadMode) :: IO (Either SomeException Handle)
    a <- case h of
           Right h' -> decodeAuto myAuto <$> B.hGetContents h'
           Left _   -> return myAuto
    a' <- loopAuto a
    mapM_ hClose h
    B.writeFile "data/state" (encodeAuto a')


loopAuto :: MonadIO m => Interact m -> m (Interact m)
loopAuto a' = do
    inp      <- liftIO $ do
                  putStr "> "
                  hFlush stdout
                  try getLine :: IO (Either SomeException String)
    case inp of
      Right inp' -> do
        (out, a) <- stepAuto a' inp'
        liftIO    $ mapM_ putStrLn out
        loopAuto a
      Left _ -> return a'

myAuto :: Monad m => Interact m
myAuto = foldr (<|>) mzero <$> sequenceA [countAuto]

countAuto :: Monad m => Interact m
countAuto = proc inp ->
    if "c " `isPrefixOf` inp
      then do
        c <- integral 0 -< 1 :: Int
        returnA -< return (show c)
      else
        returnA -< mzero




