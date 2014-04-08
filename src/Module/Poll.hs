{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoNegativeLiterals #-}

module Module.Poll (pollAuto) where

import Auto
import Control.Applicative hiding   (many)
import Control.Arrow
import Control.Category
import Control.Monad
import Data.Binary
import Data.Char
import Data.IntMap.Strict           (IntMap)
import Data.List
import Data.Map.Strict              (Map)
import Data.Monoid
import Event
import GHC.Generics
import Prelude hiding               (until, id, (.))
import Text.Parsec hiding           ((<|>))
import Text.Parsec.String
import Types
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict    as M

data Poll = Poll { _pollNick  :: String
                 , _pollPoll  :: String
                 , _pollOpts  :: IntMap String
                 , _pollVotes :: Map String Int
                 } deriving (Show, Generic)

data Vote = Vote { _voteNick :: String
                 , _voteOpt  :: Int
                 } deriving (Show, Generic)

instance Binary Poll

data PollCommand = PCReset
                 | PCClear
                 | PCSet Poll
                 | PCReport
                 | PCVote Vote
                 | PCHelp
                 deriving (Show)

reportPoll :: Poll -> [String]
reportPoll (Poll n p o v) = ["\"" ++ p ++ "\" (" ++ n ++ ")"
                            ,   intercalate "; "
                              . map (\(ok,ov) ->
                                  concat [ "(" ++ [chr (ok + ord 'a')] ++ ") "
                                         , ov ++ ": "
                                         , show (M.size . M.filter (== ok) $ v)
                                         ]
                                  )
                              . IM.toList
                              $ o
                            ]

helpMsg :: [String]
helpMsg = [ "Create and monitor polls --- @p (cmd) (opts) or @p (vote)"
          , "cmd: set question op1 op2 \"op 3\", report, reset, clear, help"
          , "vote: a, b, c, etc."
          ]

parseCommand :: String -> Parser (Maybe PollCommand)
parseCommand nick = do
    string "@p"
    many1 space
    optionMaybe . try $ do
      com <- many1 alphaNum
      spaces
      case com of
        "help"   -> return PCHelp
        "reset"  -> return PCReset
        "clear"  -> return PCClear
        "report" -> return PCReport
        "show"   -> return PCReport
        "count"  -> return PCReport
        "set"    -> PCSet <$> poll
        _        -> maybe mzero (return . PCVote . Vote nick) (opt com)
  where
    strlit =     between (char '"') (char '"') (many (noneOf "\""))
             <|> many1 (noneOf " ")
    opt (x:[]) | isAsciiUpper x = Just (ord x - ord 'A')
               | isAsciiLower x = Just (ord x - ord 'a')
               | isDigit x      = Just (digitToInt x)
    opt  _                      = Nothing
    poll =     Poll nick
           <$> try strlit
           <*> (IM.fromList . zip [0..] <$> many1 (spaces *> strlit))
           <*> pure M.empty

pollAuto :: Monad m => Interact m
pollAuto = proc (InMessage nick msg) -> do
    let parsed = parse (parseCommand nick) "" msg
    case parsed of
      Right Nothing    -> returnA -< return "Invalid poll commmand; try @p help"
      Left _           -> returnA -< mzero
      Right (Just cmd) -> do
        poll <- pollStatus -< cmd
        let out = case cmd of
                    PCReport -> maybe ["No poll currently."] reportPoll poll
                    PCHelp   -> helpMsg
                    PCClear  -> return "Poll cleared!"
                    PCReset  -> return "Voting reset!"
                    PCSet pl -> let (Poll n p o _) = pl
                                in  [ "Poll set: " ++ p ++ " (" ++ n ++ ")"
                                    ,   intercalate "; "
                                      . map (\(ok,ov) -> "(" ++ [chr (ok + ord 'a')] ++ ") " ++ ov)
                                      . IM.toList
                                      $ o
                                    ]
                    _        -> mzero
        returnA -< out

pollStatus :: forall m. Monad m => Auto m PollCommand (Maybe Poll)
pollStatus = proc cmd -> do
    let vote  = case cmd of
                  PCVote v -> Just v
                  _        -> Nothing
        clear = case cmd of
                  PCClear  -> Just ()
                  _        -> Nothing
        reset = case cmd of
                  PCReset    -> Just ()
                  PCSet _    -> Just ()
                  _          -> Nothing
        newp  = case cmd of
                  PCSet p    -> Just p
                  _          -> Nothing
    votes   <- switch voteLoop -< (vote, reset <> clear)
    poll    <- switch pollLoop -< (newp, clear)
    returnA -< setVotes votes <$> poll
  where
    setVotes v p = p { _pollVotes = v }
    pollLoop :: Auto m (Maybe Poll, Maybe ()) (Maybe Poll, Maybe (Auto m (Maybe Poll, Maybe ()) (Maybe Poll)))
    pollLoop = proc (newp, clear) -> do
      poll <- hold -< newp
      let next = switch pollLoop <$ clear
          poll' = maybe poll (const Nothing) next
      returnA -< (poll', next)
    voteLoop :: Auto m (Maybe Vote, Maybe ()) (Map String Int, Maybe (Auto m (Maybe Vote, Maybe ()) (Map String Int)))
    voteLoop = proc (vote, reset) -> do
      votes <- scanE addVote M.empty -< vote
      let next = switch voteLoop <$ reset
          votes' = maybe votes (const M.empty) next
      returnA -< (votes', next)
    addVote m (Vote n i) = M.insert n i m
