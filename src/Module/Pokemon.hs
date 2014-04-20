{-# LANGUAGE Arrows #-}

module Module.Pokemon where

import Auto
import Data.List
import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Random
import Data.IntMap.Strict     (IntMap)
import Data.Maybe
import Data.Monoid
import Data.Pokemon
import Data.Pokemon.Load
import Data.Pokemon.PalPark
import Data.Time
import Data.Time.Clock.POSIX
import Event
import Text.Parsec
import Text.Parsec.String
import Types
import qualified Data.IntMap  as IM
import qualified Data.Text    as T

type PMPD = ((IntMap Species, IntMap Move), IntMap (PalArea, Int))

databasedir :: FilePath
databasedir = "../pokemon/data"

data PokeCommand = PCCatch
                 | PCCheck
                 deriving Show

parseCommand :: Parser PokeCommand
parseCommand = do
    wrd <- many1 alphaNum
    case wrd of
      "catch" -> return PCCatch
      "list"  -> return PCCheck
      "check" -> return PCCheck
      _       -> mzero

pokeAuto :: MonadIO m => Interact' m
pokeAuto = proc im@(InMessage nick msg _ _) -> do
    case words msg of
      h:msgw' | h `elem` ["@pk","@pkmn"] -> do
        pkmnsmoves <- cacheAuto (liftIO (loadData databasedir))    -< ()
        paldata'   <- cacheAuto (liftIO (loadPalPark databasedir)) -< ()
        let pmpd' = (,) <$> pkmnsmoves <*> paldata'
        case pmpd' of
          Nothing ->
            returnA -< mzero
          Just pmpd -> do
            let im' = im { inMessageBody = unwords msgw' }
            o <- multiAuto (const personalAuto) -< (nick, (im', pmpd))
            returnA -< o
      _ -> returnA -< mzero

-- catch

personalAuto :: Monad m => Auto m (InMessage, PMPD) [String]
personalAuto = proc (InMessage nick msg _ t, ((specs,moves),pals)) -> do
    let parsed = parse parseCommand "" msg
    case parsed of
      Right comm -> do
        let gen    = mkStdGen . round . utcTimeToPOSIXSeconds $ t
            palmap = palAreas pals
            caught = case comm of
                       PCCatch -> flip evalRand gen $ do
                         caughtany <- getRandomR (1 :: Int,10)
                         if caughtany < 7
                           then do
                             region <- (palAreaList !!) <$> getRandomR (0,length palAreaList - 1)
                             pid    <- randPal region palmap
                             return (Just (specs IM.! pid))
                           else
                             return Nothing
                       _       -> Nothing
            caughtmsg = case (comm,caught) of
                          (PCCatch, Nothing) -> [nick ++ ": Nothing found, sorry."]
                          (PCCatch, Just sp) -> [nick ++ ": Caught a " ++ T.unpack (_speciesName sp) ++ "!"]
                          _                  -> []
        pokes <- scanE (<>) mempty -< (:[]) <$> caught
        let checkmsg = case comm of
                         PCCheck -> [ nick ++ ": You have " ++ show numpokes ++ " pokemon:" ]
                                    ++ if numpokes > 0
                                         then [ nick ++ ": " ++ intercalate ", " pokenames ]
                                         else []
                         _       -> mzero
            numpokes = length pokes
            pokenames = map (T.unpack . _speciesName) pokes
            outs = caughtmsg <> checkmsg
        returnA -< outs
      Left _  ->
        returnA -< mzero
    


