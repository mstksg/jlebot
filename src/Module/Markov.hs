{-# LANGUAGE Arrows #-}

module Module.Markov (markovAuto) where

-- import Control.Monad          (mzero)
-- import Data.Traversable
-- import System.Random
import Auto
import Control.Applicative
import Control.Arrow
import Control.Category
import Control.Monad.Loops
import Control.Monad.Random
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Char
import Data.Foldable
import Data.List hiding          (sum, foldr)
import Data.Map.Strict           (Map)
import Data.Maybe
import Data.Time
import Event
import Prelude hiding            ((.), foldr, sum, sequence)
import Types
import qualified Data.Map.Strict as M

memory :: Int
memory = 3

type Training = Map String (Map Char Int)

hash :: String -> Int
hash = sum . map ord

markovAuto :: Monad m => Interact' m
markovAuto = proc (InMessage nick msg _ t) -> do
    trainings <- gatherAuto (const trainingAuto) -< (nick, msg)
    let tgen = (+ hash (nick ++ msg))
             . round
             . (* 1000)
             . utctDayTime
             $ t
        makeMarkov' :: Maybe (String, Int)
        makeMarkov' = case words msg of
                        "@markov":n:seed:[] -> event (n, hash seed)
                        "@markov":n:_       -> event (n, tgen)
                        "@impersonate":n:seed:[] -> event (n, hash seed)
                        "@impersonate":n:_       -> event (n, tgen)
                        _                   -> noEvent
        makeMarkov :: Maybe (String, StdGen)
        makeMarkov  = second mkStdGen <$> makeMarkov'
        out :: Maybe String
        out = (\(n, seed) -> let m = M.lookup n trainings
                             in  case m of
                                   Just m' -> let res = evalRand (genMarkov m') seed
                                              in  if null res
                                                    then "Not enough information on user " ++ n
                                                    else "<" ++ n ++ "> " ++ res
                                   Nothing -> "No memory of user " ++ n
              ) <$> makeMarkov

    returnA -< maybeToList out

trainingAuto :: Monad m
             => Auto m String Training
trainingAuto = scanA add M.empty . arr makeAdds
  where
    add :: Training
        -> [(String, Char)]
        -> Training
    add = foldr f
      where
        f :: (String, Char)
          -> Training
          -> Training
        f (s, c) = M.insertWith (M.unionWith (+)) s (M.singleton c 1)

makeAdds :: String -> [(String, Char)]
makeAdds ('@':_)     = []
makeAdds ('>':'>':_) = []
makeAdds ('.':_)     = []
makeAdds ('s':'/':_) = []
makeAdds xs          = map pullLast
                     . transpose
                     . dropchain
                     . reverse . (++ "\0")
                     $ xs
  where
      pullLast []     = ([],'\0')
      pullLast (y:ys) = (reverse ys, y)
      dropchain ys    = map (`drop` ys) [0..memory]

genMarkov :: Training -> Rand StdGen String
genMarkov m = evalStateT (unfoldM go) ""
  where
    go :: StateT String (Rand StdGen) (Maybe Char)
    go = do
      options <- gets (\s -> M.findWithDefault M.empty s m)
      let ml = M.toList options
      if null ml
        then return Nothing
        else do
          n <- lift . fromList . map (second fromIntegral) $ ml
          case n of
            '\0' ->
              return Nothing
            c    -> do
              modify $ reverse . take memory . reverse . (++ [c])
              return (Just c)

