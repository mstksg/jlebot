{-# LANGUAGE ScopedTypeVariables #-}

module Module.Haskell (haskAuto) where

-- import Control.Applicative
-- import Control.Arrow
-- import Control.Exception
-- import Mueval.Parallel
-- import Prelude hiding            (forM)
import Auto
import Control.Concurrent
import Control.DeepSeq
import Control.Monad hiding         (forM)
import Data.Maybe
import Data.Traversable
import Language.Haskell.Interpreter
import Mueval.ArgsParse
import Mueval.Interpreter
import Types

data IMode = IEval | IType

ops :: String -> Options
ops expr = ops' { modules  = Just [ "Prelude"
                                  , "Control.Applicative"
                                  , "Control.Monad"
                                  , "Data.List"
                                  , "Data.Char"
                                  , "Data.Typeable"
                                  , "Control.Arrow"
                                  , "Data.Monoid"
                                  , "Data.Maybe"
                                  ]
                , namedExtensions = [ "Arrows"
                                    , "ScopedTypeVariables"
                                    ]
                -- , loadFile = "misc/mueval-src/Utils.hs"
                }

  where
    Right ops' = getOptions ["--expression", expr]

-- haskAuto :: Monad m => Interact m
-- haskAuto = pure []
haskAuto :: MonadIO m => Interact' m
haskAuto = arrM $ haskInterp . inMessageBody

haskInterp :: MonadIO m => String -> m [String]
haskInterp str = do
    let imode = case listToMaybe (words str) of
                  -- Just ":t" -> Just IType
                  Just ">>"  -> Just IEval
                  _         -> Nothing

    out' <- forM imode $ \mode -> do
      let str' = parens . unwords . drop 1 . words $ str
      wait <- liftIO newEmptyMVar
      tid <- liftIO . forkIO $ do
        res <- runInterpreter (interpreter (ops str'))
        out <- case res of
                    Right x -> x `deepseq` return res
                    Left _  -> return res
        void $ tryPutMVar wait (out `seq` Just out)

      liftIO . forkIO $ do
        threadDelay 700000
        killThread tid
        void $ tryPutMVar wait Nothing

      res <- liftIO $ takeMVar wait

      return $ case res of
                Just (Left e)  -> ["Error: " ++ (unwords . take 1 . words . show) e]
                Just (Right (_,t,r)) -> case mode of
                                          IEval -> [r]
                                          IType -> [t]
                Nothing        -> ["Error: Timed out."]

    return $ fromMaybe [] out'

-- trunc n s | length s > n = take n s ++ "..."
--           | otherwise    = s
