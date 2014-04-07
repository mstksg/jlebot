module Types where

import Auto

data InMessage = InMessage { inMessageNick :: String
                           , inMessageBody :: String
                           }

type Interact m = Auto m InMessage [String]
