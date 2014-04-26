{-# LANGUAGE Arrows #-}
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
import Prelude hiding      ((.), id)
import System.Random
import Types

maxWrong :: Int
maxWrong = 10

data HMCommand = HMGuess Char
               | HMShow
               | HMNew
               | HMHelp
               deriving (Show, Eq)

data PuzzleStatus = PuzzleActive
                  | PuzzleSolved
                  | PuzzleFailure [Char]
                  deriving (Show, Eq, Ord, Generic)

instance Binary PuzzleStatus

data Puzzle = Puzzle { _puzzleStr    :: String
                     , _puzzleMisses :: [Char]
                     , _puzzleStatus :: PuzzleStatus
                     } deriving (Show, Eq)

hangmanAuto :: Monad m => Interact' m
hangmanAuto = multiAuto (const roomAuto) . (arr inMessageSource &&& id)

roomAuto :: Monad m => Interact' m
roomAuto = proc (InMessage nick msg _ t) -> do
    phrasePool <- scanE (concLim 100) [] -< mfilter validPhrase . return . unwords . words . map toUpper $ msg
    case words msg of
      "@hm":commstr -> do
        let comm = parseCommand commstr
            gen  = mkStdGen
                 . (+ sum (map ord (nick ++ msg)))
                 . round
                 . (* 1000)
                 . utctDayTime
                 $ t
            randPhrase | null phrasePool = Nothing
                       | otherwise       = Just . (phrasePool !!) . fst
                                           . randomR (0, length phrasePool - 1)
                                           $ gen
            obscPhr = map (uncover []) <$> randPhrase
            newPuzz = Puzzle <$> obscPhr <*> pure [] <*> pure PuzzleActive
        case comm of
          Just comm' -> do
            puzz <- switch (puzzleAuto Nothing) -< (comm', randPhrase)
            returnA -< case (puzz, comm') of
              (_, HMHelp)  -> return "It's simple. We solve the hangman.  @hm (c) to guess, @hm show to show status, @hm new for new game."
              (_, HMNew)   | null phrasePool -> return "Phrase dictionary empty.  Try again later."
                           | otherwise       -> return "New game created!"
                                             ++ maybeToList (displayPuzzle <$> newPuzz)
              (Nothing, _) -> return "No game."
              (Just p , _) -> return (displayPuzzle p)
          _          -> returnA -< return "Invalid hangman commmand.  Try @hm help for more information."
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
           _               -> m
    displayPrefix PuzzleActive = "Active:"
    displayPrefix PuzzleSolved = "Solved!"
    displayPrefix (PuzzleFailure _) = "Failure!"

puzzleAuto :: Monad m => Maybe String -> Auto m (HMCommand, Maybe String) (Maybe Puzzle, Event (Auto m (HMCommand, Maybe String) (Maybe Puzzle)))
puzzleAuto str0 = proc (comm, newphrase) -> do
    let newPuzz = case comm of
                    HMNew -> event (switch (puzzleAuto newphrase))
                    _     -> noEvent
        guesscorr = case comm of
                    HMGuess c -> ((toUpper c `elem`) &&& const (toUpper c)) <$> str0
                    _         -> noEvent
        cguess    = snd <$> mfilter fst guesscorr
        wguess    = snd <$> mfilter (not . fst) guesscorr
    cguesses <- scanE (\xs x -> nub (x:xs)) []                   -< cguess
    wguesses <- arr reverse
              . scanE (\xs x -> take maxWrong . nub $ (x:xs)) [] -< wguess
    let wrongs  = length wguesses
        solved  = isJust . mfilter (all (`elem` (' ':cguesses))) $ str0
        solvedE = whenE solved PuzzleSolved
        failedE = whenE (wrongs >= maxWrong) (PuzzleFailure wguesses)
    status <- arr (fromMaybe PuzzleActive)
            . scanA (<|>) Nothing          -< failedE <|> solvedE
    
    let strout = case status of
                   PuzzleActive -> map (uncover cguesses) <$> str0
                   _            -> str0
        puzz   = Puzzle
             <$> strout
             <*> pure wguesses
             <*> pure status
    returnA -< (puzz, newPuzz)


uncover :: [Char] -> Char -> Char
uncover guesses c | c `elem` guesses = c
                  | c `elem` " "     = ' '
                  | otherwise        = '_'

validPhrase :: String -> Bool
validPhrase (words->str) = validLength (length str) && notComm str
  where
    validLength x = x > 2 && x < 5
    notComm (('@':_):_) = False
    notComm _           = True

concLim :: Int -> [a] -> a -> [a]
concLim n xs x = take n (x:xs)

parseCommand :: [String] -> Maybe HMCommand
parseCommand ((g:[]):[]) = Just (HMGuess g)
parseCommand ("show":[]) = Just HMShow
parseCommand ("help":[]) = Just HMHelp
parseCommand ("new":[])  = Just HMNew
parseCommand _           = Nothing

