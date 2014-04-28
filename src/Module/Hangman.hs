{-# LANGUAGE Arrows #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveGeneric #-}

module Module.Hangman (hangmanAuto) where

-- import Data.Monoid
import Auto
import Control.Applicative
import Control.Arrow
import Control.Category
import Control.Monad
import Data.Binary
import Data.Char
import Data.List
import Data.Maybe
import Data.Time
import Event
import GHC.Generics
import Prelude hiding            ((.), id)
import System.Random
import Types
import qualified Data.Map.Strict as M

maxWrong :: Int
maxWrong = 10

poolSize :: Int
poolSize = 500

data HMCommand = HMGuess Char
               | HMSolve String
               | HMShow
               | HMNew
               | HMHelp
               deriving (Show, Eq)

data PuzzleStatus = PuzzleActive
                  | PuzzleSolved String
                  | PuzzleFailure String
                  deriving (Show, Eq, Ord, Generic)

instance Binary PuzzleStatus

data Puzzle = Puzzle { _puzzleStr    :: String
                     , _puzzleMisses :: String
                     , _puzzleStatus :: PuzzleStatus
                     } deriving (Show, Eq)

toPhrase :: String -> Maybe String
toPhrase = mfilter validPhrase . return . unwords . words . map toUpper . notComm
  where
    notComm ('@':_) = ""
    notComm s       = s

hangmanAuto :: Monad m => Interact m
hangmanAuto = proc im@(InMessage _ msg src _) -> do
    pool <- scanE (concLim poolSize) [] -< toPhrase msg
    o    <- multiAuto (const roomAuto)  -< (src, (im, pool))
    returnA -< OutMessages $ M.singleton src o

roomAuto :: Monad m => Auto m (InMessage, [String]) [String]
roomAuto = proc (InMessage nick msg _ t, globalPool) -> do
    phrasePool <- scanE (concLim poolSize) [] -< toPhrase msg
    let totalPool | null phrasePool = globalPool
                  | otherwise       = phrasePool

    case words msg of
      "@hm":commstr -> do
        let comm = parseCommand commstr
            gen  = mkStdGen
                 . (+ sum (map ord (nick ++ msg)))
                 . round
                 . (* 1000)
                 . utctDayTime
                 $ t
            randPhrase | null totalPool = Nothing
                       | otherwise      = Just . (totalPool !!) . fst
                                          . randomR (0, length totalPool - 1)
                                          $ gen
            obscPhr = map (uncover []) <$> randPhrase
            newPuzz = Puzzle <$> obscPhr <*> pure [] <*> pure PuzzleActive
        puzz <- switch (puzzleAuto Nothing) -< (comm, randPhrase)
        returnA -< case (puzz, comm) of
          (_, HMHelp)  -> return "It's simple. We solve the hangman.  @hm (c) to guess, @hm show to show status, @hm new for new game."
          (_, HMNew)   | null totalPool -> return "Phrase dictionary empty.  Try again later."
                       | otherwise      -> return "New game created!"
                                        ++ maybeToList (displayPuzzle <$> newPuzz)
          (Nothing, _) -> return "No game. @hm new for new game."
          (Just p , _) -> return (displayPuzzle p)
      _                 -> returnA -< mzero

displayPuzzle :: Puzzle -> String
displayPuzzle (Puzzle s m p) = displayPrefix p
                            ++ " ["
                            ++ s
                            ++ "] ("
                            ++ m'
                            ++ replicate (maxWrong - length m') '.'
                            ++ ")"
  where
    m' = case p of
           PuzzleFailure w -> w
           PuzzleSolved w  -> w
           _               -> m
    displayPrefix PuzzleActive      = "Active:"
    displayPrefix (PuzzleSolved _)  = "Solved!"
    displayPrefix (PuzzleFailure _) = "Failure!"

puzzleAuto :: forall m. Monad m => Maybe String -> Auto m (HMCommand, Maybe String) (Maybe Puzzle, Event (Auto m (HMCommand, Maybe String) (Maybe Puzzle)))
puzzleAuto str0 = proc (comm, newphrase) -> do
    let newPuzz = case comm of
                    HMNew -> event (switch (puzzleAuto newphrase))
                    _     -> noEvent

        guesscorr :: Event (Bool, Char)
        guesscorr = case comm of
                      HMGuess c -> ((c `elem`) &&& const c) <$> str0
                      _         -> noEvent
        cguess :: Event Char
        cguess    = snd <$> mfilter fst         guesscorr
        wguess :: Event Char
        wguess    = snd <$> mfilter (not . fst) guesscorr

        solvedcorr :: Event Bool
        solvedcorr = case comm of
                       HMSolve s -> (s ==) <$> str0
                       _         -> noEvent

        wsolve :: Event Char
        wsolve    = '*' <$ mfilter not solvedcorr

        csolve :: Event ()
        csolve    = ()  <$ mfilter id solvedcorr


    cguesses <- scanE (\xs x -> nub' x (x:xs)) [] -< cguess
    wguesses <- reverse
            <$> scanE (\xs x -> nub' x (x:xs)) [] -< wguess <|> wsolve

    let wrongs   = length wguesses
        guessall = isJust . mfilter (all (`elem` (' ':cguesses))) $ str0
        solved   = guessall || isJust csolve
        solvedE  = PuzzleSolved wguesses  <$ guard solved
        failedE  = PuzzleFailure wguesses <$ guard (wrongs >= maxWrong)

    status <- fromMaybe PuzzleActive
          <$> scanA (<|>) Nothing    -< failedE <|> solvedE

    let strout = case status of
                   PuzzleActive -> map (uncover cguesses) <$> str0
                   _            -> str0
        puzz   = Puzzle <$> strout <*> pure wguesses <*> pure status
    returnA -< (puzz, newPuzz)

nub' :: Char -> String -> String
nub' '*' = id
nub' _   = nub

uncover :: String -> Char -> Char
uncover guesses c | c `elem` guesses = c
                  | not (isAlpha c)  = c
                  | otherwise        = '_'

validPhrase :: String -> Bool
validPhrase (words->str) = validLength (length str)
  where
    validLength x = x > 2 && x <= 6

concLim :: Int -> [a] -> a -> [a]
concLim n xs x = take n (x:xs)

parseCommand :: [String] -> HMCommand
parseCommand ((g:[]):[]) | isAlphaNum g = HMGuess . toUpper $ g
parseCommand ("show":[]) = HMShow
parseCommand ("help":[]) = HMHelp
parseCommand ("new":[])  = HMNew
parseCommand s           = HMSolve . map toUpper . unwords $ s

