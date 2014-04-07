{-# LANGUAGE Arrows #-}
{-# LANGUAGE NoNegativeLiterals #-}

module Module.Poll (pollAuto) where

import Control.Applicative hiding   (many)
import Control.Arrow
import Control.Monad
import Data.Char
import Data.IntMap.Strict           (IntMap)
import Text.Parsec hiding           ((<|>))
import Text.Parsec.Error
import Text.Parsec.String
import Types
import qualified Data.IntMap.Strict as IM

data PollCommand = PCReset
                 | PCSet { pcSetNick :: String
                         , pcSetPoll :: String
                         , pcSetOpts :: (IntMap String)
                         }
                 | PCReport
                 | PCVote { pcVoteNick :: String
                          , pcVoteOpt  :: Int
                          }
                 | PCHelp
                 deriving (Show)


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
        "report" -> return PCReport
        "count"  -> return PCReport
        "set"    -> PCSet nick
                      <$> strlit
                      <*> (IM.fromList . zip [0..] <$> many1 (spaces *> strlit))
        _        -> maybe mzero (return . PCVote nick) (opt com)
  where
    strlit = between (char '"') (char '"') (many (noneOf "\""))
    opt (x:[]) | isAsciiUpper x = Just (ord x - ord 'A')
               | isAsciiLower x = Just (ord x - ord 'a')
               | isDigit x      = Just (digitToInt x)
    opt  _                      = Nothing

pollAuto :: Monad m => Interact m
pollAuto = proc (InMessage nick msg) -> do
    let parsed = parse (parseCommand nick) "" msg
    case parsed of
      Right (Just cmd) -> returnA -< return (show cmd)
      Right Nothing    -> returnA -< return "Invalid poll commmand; try @p help"
      Left _           -> returnA -< mzero

