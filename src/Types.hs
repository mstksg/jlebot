module Types where

import Auto
import Data.Map.Strict (Map, unionsWith, unionWith)
import Data.Time
import Data.Monoid

data InMessage = InMessage { inMessageNick   :: String
                           , inMessageBody   :: String
                           , inMessageSource :: String
                           , inMessageTime   :: UTCTime
                           }

newtype OutMessages = OutMessages { outMessageMap :: Map String [String]
                                  }

instance Monoid OutMessages where
    mempty = OutMessages mempty
    mappend (OutMessages a) (OutMessages b) = OutMessages (unionWith (<>) a b)

type Interact m = Auto m InMessage OutMessages

type Interact' m = Auto m InMessage [String]

combineOutMessages :: [OutMessages] -> OutMessages
combineOutMessages = OutMessages . unionsWith (<>) . map outMessageMap
