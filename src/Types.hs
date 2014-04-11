{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types where

import Auto
import Data.Map.Strict
import Data.Time
import Data.Monoid

data InMessage = InMessage { inMessageNick   :: String
                           , inMessageBody   :: String
                           , inMessageSource :: String
                           , inMessageTime   :: UTCTime
                           }

newtype OutMessages = OutMessages { outMessageMap :: Map String [String]
                                  } deriving (Monoid)

type Interact m = Auto m InMessage OutMessages

type Interact' m = Auto m InMessage [String]
