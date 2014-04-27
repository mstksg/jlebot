{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Module.Cryptogram (cryptogramAuto) where

-- import Control.Monad.Fix
import Data.Ord
import Auto
import Control.Monad.Random
import Debug.Trace
import Control.Applicative
import Control.Arrow
import Control.Monad
import GHC.Generics
import Data.Binary
import Data.Char
import Data.Digest.Pure.MD5
import Data.List
import Data.Map.Strict           (Map)
import Data.Maybe
import Data.Monoid
import Data.Time
import Event
import System.Random
import Types
import qualified Data.Map.Strict as M

poolSize :: Int
poolSize = 500

takebackLimit :: Int
takebackLimit = 10

type Permutation = Map Char Char

data CGCommand = CGSub Char Char
               | CGUnsub Char
               | CGNew
               | CGShow
               | CGHelp
               deriving (Show, Eq)

data Puzzle = Puzzle { _puzzleStr :: String
                     , _puzzleMap :: Permutation
                     , _puzzleBTs :: Int
                     , _puzzleStatus :: PuzzleStatus
                     } deriving (Show, Eq)

data PuzzleStatus = PuzzleActive
                  | PuzzleSolved Permutation
                  | PuzzleFailure Permutation
                  deriving (Show, Eq, Generic)

instance Binary PuzzleStatus

toPhrase :: String -> Maybe String
toPhrase = mfilter validPhrase . return . unwords . words . unwords . fmap (filter isAlpha) . words . map toUpper

parseCommand :: [String] -> Maybe CGCommand
parseCommand ((x:[]):(y:[]):[]) | isAlpha x && isAlpha y = Just (CGSub x y)
parseCommand (('!':x:[]):[])    | isAlpha x              = Just (CGUnsub x)
parseCommand ("show":[]) = Just CGShow
parseCommand ("help":[]) = Just CGHelp
parseCommand ("new":[])  = Just CGNew
parseCommand _           = Nothing


cryptogramAuto :: Monad m => Interact m
cryptogramAuto = proc im@(InMessage _ msg src _) -> do
    pool <- scanE (concLim poolSize) [] -< toPhrase msg
    o    <- multiAuto (const roomAuto)  -< (src, (im, pool))
    returnA -< OutMessages $ M.singleton src o

roomAuto :: Monad m => Auto m (InMessage, [String]) [String]
roomAuto = proc (InMessage nick msg _ t, globalPool) -> do
    phrasePool <- scanE (concLim poolSize) [] -< toPhrase msg
    let totalPool | null phrasePool = globalPool
                  | otherwise       = phrasePool
    case words msg of
      "@cg":commstr -> do
        let comm = parseCommand commstr
            gen  = mkStdGen
                 . (+ sum (map ord (nick ++ msg)))
                 . round
                 . (* 1000)
                 . utctDayTime
                 $ t
            randPhrase :: Maybe (String, Permutation)
            randPhrase | null totalPool = Nothing
                       | otherwise      = Just . flip evalRand gen $ do
                                            phr <- getRandomR (0, length totalPool - 1)
                                            perm <- randPerm
                                            return (totalPool !! phr, perm)
            strout = (uncurry . flip) encodeString <$> randPhrase
            newPuzz = Puzzle <$> strout <*> pure mempty <*> pure 0 <*> pure PuzzleActive
        case comm of
          Just comm' -> do
            puzz <- switch (puzzleAuto Nothing) -< (comm', randPhrase)
            returnA -< case (puzz, comm') of
              (_, CGHelp)  -> return "It's simple. We solve the cryptogram.  @cg new for a new game.  @cg a b to propose a substitution of a for b.  @cg !a to unpropose a's substitution. @cg show to show status."
              (_, CGNew)   | null totalPool -> return "Phrase dictionary empty.  Try again later."
                           | otherwise      -> return "New game created!"
                                            ++ maybeToList (displayPuzzle <$> newPuzz)
              (Nothing, _) -> return "No game."
              (Just p , _) -> return (displayPuzzle p)
          _          -> returnA -< return "Invalid cryptogram commmand.  Try @cg help for more information."
      _                 -> returnA -< mzero

displayPuzzle :: Puzzle -> String
displayPuzzle (Puzzle s p i st) = displayPrefix st
                               ++ " [" ++ s ++ "] "
                               ++ displayPerm p' ++ " "
                               ++ show i ++ "/" ++ show takebackLimit
  where
    p' = case st of
           PuzzleSolved p'' -> p''
           PuzzleFailure p'' -> p''
           _ -> p
    displayPrefix PuzzleActive = "Active:"
    displayPrefix (PuzzleSolved _) = "Solved!"
    displayPrefix (PuzzleFailure _) = "Failure!"
    displayPerm = concat . map (\(k,v) -> k:v:[]) . M.toList

puzzleAuto :: forall m. Monad m => Maybe (String, Permutation) -> Auto m (CGCommand, Maybe (String, Permutation)) (Maybe Puzzle, Event (Auto m (CGCommand, Maybe (String, Permutation)) (Maybe Puzzle)))
puzzleAuto strperm = proc (comm, newphrasegen) -> do
    let newPuzz = case comm of
                    CGNew -> event (switch (puzzleAuto newphrasegen))
                    _     -> noEvent
        mapEvt  = case comm of
                    CGSub x y -> event (toLower x, Just (toUpper y))
                    CGUnsub x -> event (toLower x, Nothing)
                    _         -> noEvent

    subHist <- scanE (\xs x -> sort (x:xs)) [] -< fst <$> mapEvt
    subMap  <- scanE (uncurry . addMap) mempty -< mapEvt

    let numWrong = min (takebackLimit + 1) . sum . map (pred . length) . group $ subHist
        origStr = fst <$> strperm
        subStr  = encodeString subMap . (uncurry . flip) encodeString <$> strperm
        solved  = subStr == origStr
        failedE = PuzzleFailure subMap <$ guard (numWrong > takebackLimit)
        solvedE = PuzzleSolved subMap <$ guard solved

    status <- fromMaybe PuzzleActive
          <$> scanA (<|>) Nothing    -< failedE <|> solvedE

    let strout = case status of
                   PuzzleActive -> subStr
                   _            -> origStr
        puzz = Puzzle <$> strout <*> pure subMap <*> pure numWrong <*> pure status

    returnA -< (puzz, newPuzz)

encodeString :: Permutation -> String -> String
encodeString p = map (\c -> M.findWithDefault c c p)

addMap :: Map Char Char -> Char -> Maybe Char -> Map Char Char
addMap m x (Just y) = M.insert x y m
addMap m x Nothing  = M.delete x m

randPerm :: Rand StdGen Permutation
randPerm = do
    weights <- mapM (\x -> (x,) <$> getRandom) ['a'..'z']
    let shuffled  = sortBy (comparing snd) weights :: [(Char, Double)]
        shuffled' = map fst shuffled
        perm      = M.fromList $ zip ['A'..'Z'] shuffled'
    return perm

concLim :: Int -> [a] -> a -> [a]
concLim n xs x = take n (x:xs)

validPhrase :: String -> Bool
validPhrase (words->str) = validLength (length str) && notComm str
  where
    validLength = (> 4)
    notComm (('@':_):_) = False
    notComm _           = True

