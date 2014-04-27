{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Module.Cryptogram (cryptogramAuto) where

-- import Control.Monad.Fix
import Auto
import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.Random
import Data.Binary
import Data.Char
import Data.Digest.Pure.MD5
import Data.List
import Data.Map.Strict           (Map)
import Data.Maybe
import Data.Monoid
import Data.Ord
import Data.Time
import Debug.Trace
import Event
import GHC.Generics
import System.Random
import Text.Parsec hiding        ((<|>))
import Text.Parsec.String
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

parseCommand :: Parser [CGCommand]
parseCommand = do
    cont <- optionMaybe (try commCont)
    case cont of
      Just c  -> return [c]
      Nothing -> sepBy1 (commUnsub <|> commSub) spaces
  where
    -- anyComm  = commCont <|> commUnsub <|> commSub
    commCont = choice [ CGShow <$ string "show"
                      , CGHelp <$ string "help"
                      , CGNew  <$ string "new"
                      ]
    commUnsub = CGUnsub <$> (char '!' *> satisfy isAlpha)
    commSub   = CGSub <$> satisfy isAlpha <*> (spaces *> satisfy isAlpha)


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
        let comm = parse (parseCommand) "" (unwords commstr)
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
          Right comm' -> do
            puzz <- switch (puzzleAuto Nothing) -< (comm', randPhrase)
            returnA -< case (puzz, listToMaybe comm') of
              (_, Just CGHelp)  -> return "It's simple. We solve the cryptogram.  @cg new for a new game.  @cg a b to propose a substitution of a for b.  @cg !a to unpropose a's substitution. @cg show to show status."
              (_, Just CGNew)   | null totalPool -> return "Phrase dictionary empty.  Try again later."
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
    displayPrefix PuzzleActive      = "Active:"
    displayPrefix (PuzzleSolved _)  = "Solved!"
    displayPrefix (PuzzleFailure _) = "Failure!"
    displayPerm = concat . map (\(k,v) -> k:v:[]) . M.toList

puzzleAuto :: forall m. Monad m => Maybe (String, Permutation) -> Auto m ([CGCommand], Maybe (String, Permutation)) (Maybe Puzzle, Event (Auto m ([CGCommand], Maybe (String, Permutation)) (Maybe Puzzle)))
puzzleAuto strperm = proc (comms, newphrasegen) -> do
    let newPuzz = case listToMaybe comms of
                    Just CGNew -> event (switch (puzzleAuto newphrasegen))
                    _          -> noEvent
        mapEvts = mapM mapEvt comms

    subHist <- accelAuto (scanE (flip (:)) []) -< mapEvts
    subMap  <- accelAuto (scanE (uncurry . addMap) mempty) -< mapEvts

    let rptFst   = repeats . map fst $ subHist
        rptSnd   = repeats . map snd $ subHist
        numWrong = min (takebackLimit + 1) (rptFst + rptSnd)
        origStr  = fst <$> strperm
        subStr   = encodeString subMap . (uncurry . flip) encodeString <$> strperm
        solved   = subStr == origStr
        failedE  = PuzzleFailure subMap <$ guard (numWrong > takebackLimit)
        solvedE  = PuzzleSolved subMap  <$ guard solved

    -- create "first event"
    status <- fromMaybe PuzzleActive <$> scanA (<|>) Nothing -< failedE <|> solvedE

    let strout = case status of
                   PuzzleActive -> subStr
                   _            -> origStr
        puzz = Puzzle <$> strout <*> pure subMap <*> pure numWrong <*> pure status

    returnA -< (puzz, newPuzz)
  where
    mapEvt comm = case comm of
                    CGSub x y -> event (toLower x, Just (toUpper y))
                    CGUnsub x -> event (toLower x, Nothing)
                    _         -> noEvent

repeats :: Ord a => [a] -> Int
repeats = sum . map (pred . length) . group . sort

encodeString :: Permutation -> String -> String
encodeString p = map (\c -> M.findWithDefault c c p)

addMap :: Map Char Char -> Char -> Maybe Char -> Map Char Char
addMap m x (Just y) = M.insert x y . M.filter (/= y) $ m
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

