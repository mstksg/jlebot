{-# LANGUAGE Arrows #-}

module Main where

import Auto
import Prelude hiding (mapM_, sequence, foldr, concat)
import Control.Monad hiding (mapM_)
import Network
import Data.List hiding (foldr, concat)
import Data.Traversable
import System.IO
import Control.Applicative
import Control.Exception
import qualified Data.ByteString.Lazy as B
import Control.Arrow
import Control.Monad.IO.Class
import Data.Foldable

type Interact m = Auto m String [String]

main :: IO ()
main = stdinLoop

networkLoop :: IO ()
networkLoop = do
    h <- connectTo "irc.freenode.org" (PortNumber 6667)
    hSetBuffering h NoBuffering
    write h "NICK" "jlebot"
    write h "USER" "jlebot 0 * :jle bot"
    write h "JOIN" "#jlebot-test"
    listen h

write :: Handle -> String -> String -> IO ()
write h s t = do
    hPutStrLn h $ "> " ++ s ++ " " ++ t ++ "\r"
    putStrLn    $ "> " ++ s ++ " " ++ t

listen :: Handle -> IO ()
listen h = forever $ do
    s <- hGetLine h
    putStrLn s


stdinLoop :: IO ()
stdinLoop = do
    h <- openFile "data/state" ReadMode
    hSetBuffering h NoBuffering
    loaded <- try (B.hGetContents h) :: IO (Either SomeException B.ByteString)
    let a = case loaded of
              Right bs -> decodeAuto myAuto bs
              Left _   -> myAuto
    a' <- loopAuto a
    hClose h
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




