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
memory = 5

minLength :: Int
minLength = 20

maxTries :: Int
maxTries = 50

type Training = Map String (Map Char Int)

hash :: String -> Int
hash = sum . map ord

markovAuto :: Monad m => Interact' m
markovAuto = proc (InMessage nick msg _ t) -> do
    trainings <- gatherAuto (const trainingAuto) -< (nick, msg)
    let -- random seed based on time
        tgen :: Int
        tgen = (+ hash (nick ++ msg))
             . round
             . (* 1000)
             . utctDayTime
             $ t
        makeMarkov' :: Event (String, Int)
        makeMarkov' = case words msg of
                        "@markov":n:seed:_      -> event (n, hash seed)
                        "@markov":n:_           -> event (n, tgen)
                        "@impersonate":n:seed:_ -> event (n, hash seed)
                        "@impersonate":n:_      -> event (n, tgen)
                        _                       -> noEvent
        makeMarkov :: Event (String, StdGen)
        makeMarkov  = second mkStdGen <$> makeMarkov'
        out :: Event String
        out = (\(n, seed) ->
                  let m = M.lookup n trainings
                  in  case m of
                        Just m' ->
                          let res = evalRand (genMarkovN minLength m') seed
                          in  if null res
                                then "Not enough information on user " ++ n
                                else "<" ++ n ++ "> " ++ res
                        Nothing ->
                          "No memory of user " ++ n
              ) <$> makeMarkov

    returnA -< maybeToList out

trainingAuto :: Monad m => Auto m String Training
trainingAuto = scanA (foldr add) M.empty . arr makeAdds
  where
    add :: (String, Char) -> Training -> Training
    add (s, c) = M.insertWith (M.unionWith (+)) s (M.singleton c 1)

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

genMarkovN :: Int -> Training -> Rand StdGen String
genMarkovN i m = go maxTries
  where
    go 0 = genMarkov m
    go n = do
      mkv <- genMarkov m
      if length mkv >= i
        then return mkv
        else go (n-1)

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

