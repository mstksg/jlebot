{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Module.Cryptogram (cryptogramAuto) where

import Auto
import Control.Category
import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.Random
import Data.Binary
import Control.Monad.IO.Class
import Prelude hiding ((.), id)
import Data.Char
import Data.List
import Data.Map.Strict           (Map)
import Data.Maybe
import Data.Monoid
import Data.Ord
import Data.Time
import Event
import GHC.Generics
import Text.Parsec hiding        ((<|>), many, optional)
import Text.Parsec.String
import Types
import qualified Data.Map.Strict as M

takebackLimit :: Int
takebackLimit = 4

type Permutation = Map Char Char

data CGCommand = CGSub Char Char
               | CGNew
               | CGShow
               | CGHelp
               deriving (Show, Eq)

data Puzzle = Puzzle { _puzzleStr :: String
                     , _puzzleWrongs :: [(Char,Char)]
                     , _puzzleStatus :: PuzzleStatus
                     } deriving (Show, Eq)

data PuzzleStatus = PuzzleActive
                  | PuzzleSolved [(Char,Char)]
                  | PuzzleFailure [(Char,Char)]
                  deriving (Show, Eq, Generic)

instance Binary PuzzleStatus

parseCommand :: Parser [CGCommand]
parseCommand = do
    cont <- optionMaybe (try commCont)
    res <- case cont of
      Just c  -> return [c]
      Nothing -> trySepBy1 commSub (many1 space)
    many anyChar
    return res
  where
    commCont = choice [ CGShow <$ string "show"
                      , CGShow <$ string "display"
                      , CGShow <$ string "status"
                      , CGHelp <$ string "help"
                      , CGNew  <$ string "new"
                      ]
    commSub   = CGSub <$> satisfy isAlpha <*> (many1 space *> satisfy isAlpha)

trySepBy1 :: Parser a -> Parser b -> Parser [a]
trySepBy1 p s = (:) <$> p <*> go
  where
    go = do
      pres <- optionMaybe (try (s *> p))
      case pres of
        Just res -> (res :) <$> go
        Nothing  -> return []

loadPhrases :: IO [String]
loadPhrases = map unlines
            . splitOn "%"
            . lines
          <$> readFile "/usr/share/games/fortunes/fortunes"
  where
    splitOn str = go [] []
      where
        go prev curr [] = curr:prev
        go prev curr (x:xs) | x == str  = go (curr:prev) [] xs
                            | otherwise = go prev (curr ++ [x]) xs


cryptogramAuto :: MonadIO m => Interact m
cryptogramAuto = proc im@(InMessage _ _ src _) -> do
    pool <- cacheAuto (liftIO loadPhrases)    -< ()
    o    <- multiAuto (const roomAuto)  -< (src, (im, pool))
    returnA -< OutMessages $ M.singleton src o

formatPhrase :: String -> String
formatPhrase = unwords . words . unlines . lines . map toUpper

roomAuto :: Monad m => Auto m (InMessage, [String]) [String]
roomAuto = proc (InMessage nick msg _ t, pool) ->
    case words msg of
      "@cg":commstr -> do
        let comm = parse parseCommand "" (unwords commstr)
            gen  = mkStdGen
                 . (+ sum (map ord (nick ++ msg)))
                 . round
                 . (* 1000)
                 . utctDayTime
                 $ t
            randPhrase :: Maybe (String, Permutation)
            randPhrase | null pool = Nothing
                       | otherwise = Just . flip evalRand gen $ do
                                       phr <- getRandomR (0, length pool - 1)
                                       perm <- randPerm
                                       return (formatPhrase (pool !! phr), perm)
            strout = (uncurry . flip) encodeString <$> randPhrase
            newPuzz = Puzzle <$> strout <*> pure mempty <*> pure PuzzleActive
        case comm of
          Right comm' -> do
            puzz <- switch (puzzleAuto Nothing) -< (comm', randPhrase)
            returnA -< case (puzz, listToMaybe comm') of
              (_, Just CGHelp)  -> return "It's simple. We solve the cryptogram.  @cg new for a new game.  @cg a b to propose a substitution of a for b.  @cg !a to unpropose a's substitution. @cg show to show status."
              (_, Just CGNew)   | null pool -> return "Phrase dictionary empty.  Try again later."
                                | otherwise -> return "New game created!"
                                            ++ maybeToList (displayPuzzle <$> newPuzz)
              (Nothing, _) -> return "No game."
              (Just p , _) -> return (displayPuzzle p)
          _          -> returnA -< return "Invalid cryptogram commmand.  Try @cg help for more information."
      _                 -> returnA -< mzero

displayPuzzle :: Puzzle -> String
displayPuzzle (Puzzle s p st) = displayPrefix st
                             ++ " [" ++ s ++ "] ("
                             ++ displayPerm p'
                             ++ replicate ((takebackLimit  - length p') * 2) '.'
                             ++ ")"
  where
    p' = case st of
           PuzzleSolved p''  -> p''
           PuzzleFailure p'' -> p''
           _                 -> p
    displayPrefix PuzzleActive      = "Active:"
    displayPrefix (PuzzleSolved _)  = "Solved!"
    displayPrefix (PuzzleFailure _) = "Failure!"
    displayPerm                     = concatMap (\(k,v) -> [k,v])

puzzleAuto :: forall m. Monad m => Maybe (String, Permutation) -> Auto m ([CGCommand], Maybe (String, Permutation)) (Maybe Puzzle, Event (Auto m ([CGCommand], Maybe (String, Permutation)) (Maybe Puzzle)))
puzzleAuto strperm = proc (comms, newphraseperm) -> do
    let str0    = fst <$> strperm
        perm0   = snd <$> strperm

        newPuzz = case listToMaybe comms of
                    Just CGNew -> event (switch (puzzleAuto newphraseperm))
                    _          -> noEvent

        guesses :: [(Bool, (Char,Char))]
        guesses = concat . maybeToList $
                    (\perm -> mapMaybe (checkGuess perm) comms) <$> perm0

        cguesses, wguesses :: [(Char,Char)]
        (cguesses, wguesses) = (map snd *** map snd) . partition fst $ guesses

    subMap <- mscanA -< M.fromList cguesses
    wrongs <- arr nub . mscanA -< wguesses

    let numWrong = length wrongs
        subStr   = encodeString subMap . (uncurry . flip) encodeString <$> strperm
        solved   = isJust . mfilter id   $ (==) <$> subStr <*> str0

        failedE  = PuzzleFailure wrongs <$ guard (numWrong > takebackLimit)
        solvedE  = PuzzleSolved  wrongs <$ guard solved

    status <- fromMaybe PuzzleActive <$> once -< failedE <|> solvedE

    let strout = case status of
                   PuzzleActive -> subStr
                   _            -> str0

        puzz = Puzzle <$> strout <*> pure wrongs <*> pure status

    returnA -< (puzz, newPuzz)
  where
    checkGuess perm (CGSub x y) = event (M.lookup (toUpper y) perm == Just (toLower x), (toLower x,toUpper y))
    checkGuess _ _ = noEvent

encodeString :: Permutation -> String -> String
encodeString p = map (\c -> M.findWithDefault c c p)

randPerm :: Rand StdGen Permutation
randPerm = do
    weights <- mapM (\x -> (x,) <$> getRandom) ['a'..'z']
    let shuffled  = sortBy (comparing snd) weights :: [(Char, Double)]
        shuffled' = map fst shuffled
        perm      = M.fromList $ zip ['A'..'Z'] shuffled'
    return perm

