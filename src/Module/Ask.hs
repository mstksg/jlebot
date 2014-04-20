module Module.Ask where

import Control.Arrow
import Data.Binary
import Data.Char
import Data.Digest.Pure.MD5
import Control.Monad
import Data.Maybe
import Data.Time
import Types
import qualified Data.ByteString.Lazy as B

askAuto :: Monad m => Interact' m
askAuto = arr ask

ask :: InMessage -> [String]
ask (InMessage nick msg _ time) = case words msg of
                                    "@ask":_:_ -> [nick ++ ": " ++ res]
                                    "jlebot:":_:_ | not (hasGreeting msg) -> [nick ++ ": " ++ res]
                                    _          -> mzero
  where
    res = cycle responses !! fromInteger bhead
    day = toModifiedJulianDay $ utctDay time
    dig = md5 $ B.concat [encode nick, encode (map toLower msg), encode day]
    bhead = toInteger . fromMaybe 0 . listToMaybe . B.unpack . encode $ dig

hasGreeting :: String -> Bool
hasGreeting str = any (`elem` strwords )
                      ["hi", "hello", "hey", "sup", "hola", "oy", "yo"]
  where
    strwords = words str

responses :: [String]
responses = [ "Yes"
            , "No"
            , "Yea ok"
            , "Probably"
            , "Most likely"
            , "Ask again"
            , "No not at all"
            , "I don't even"
            , "How could I possibly answer that"
            , "I guess"
            , "Why are you asking me"
            , "Why not?"
            , "No way Jose"
            , "Actually you might want to think about that"
            , "That's the wrong question to ask."
            , "I am."
            , "Are you kidding me"
            , "Nope."
            , "Can you rephrase that please?"
            , "Ask again later."
            , "Don't ask again later."
            , "If the stars are aligned."
            ]

