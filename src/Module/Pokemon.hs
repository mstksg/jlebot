{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}

module Module.Pokemon where

import Auto
import Data.List
import Data.Char
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
import Event
import Text.Parsec
import Text.Parsec.String
import Types
import qualified Data.IntMap  as IM
import qualified Data.Text    as T

type PMPD = ((IntMap Species, IntMap Move), IntMap (PalArea, Int))

databasedir :: FilePath
databasedir = "../pokemon/data"

data PokeCommand = PCSearch
                 | PCCheck
                 | PCPokedex Int
                 deriving Show

parseCommand :: Parser PokeCommand
parseCommand = do
    wrd <- many1 alphaNum
    case wrd of
      "search" -> return PCSearch
      "look"  -> return PCSearch
      "catch" -> return PCSearch
      "list"  -> return PCCheck
      "check" -> return PCCheck
      "show"  -> return PCCheck
      "dex"   -> PCPokedex . read <$> (spaces *> many1 digit)
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
        let gen  = mkStdGen
                 . round
                 . (* 1000)
                 . utctDayTime
                 $ t
            palmap = palAreas pals
            caughtdata = case comm of
                           PCSearch -> flip evalRand gen $ do
                             encountered <- getRandomR (1,10)
                             if encountered < (9 :: Int)
                               then do
                                region <- (palAreaList !!) <$> getRandomR (0,length palAreaList - 1)
                                pid    <- randPal region palmap
                                let found = specs IM.! pid
                                    crateint = min 5 (_speciesCatchRate found)
                                    crate = log (fromIntegral crateint) :: Double
                                croll <- getRandomR (0,crate)
                                return (Right (found, croll))
                              else
                                return (Left True)
                           _       -> Left False
            caught = fmap fst
                   . mfilter ((> 1.5) . snd)
                   . either (const Nothing) Just
                   $ caughtdata
            caughtmsg = case caughtdata of
                          Right (found, croll) -> [ nick ++ ": Found a " ++ T.unpack (_speciesName found) ++ ", attempting to catch!" ]
                                                  ++ if croll > 1
                                                       then [ nick ++ ": Caught!" ]
                                                       else [ nick ++ ": It got away!" ]
                          Left True            -> [ nick ++ ": Did not find any pokemon." ]
                          Left False           -> mzero
            dexmsg = case comm of
                       PCPokedex i ->
                        let pklu = IM.lookup i specs
                        in  case pklu of
                              Just lu -> return
                                       . unwords
                                       . lines
                                       . T.unpack
                                       . T.concat
                                       $ [ "Pokedex entry for "
                                         , _speciesName lu
                                         , " ("
                                         , T.pack (show i)
                                         , "): "
                                         , _speciesDescription lu
                                         ]
                              Nothing -> [ "Pokemon " ++ show i ++ " not found." ]
        pokes <- scanE (<>) mempty -< (:[]) <$> caught
        let checkmsg = case comm of
                         PCCheck -> [ nick ++ ": You have " ++ show numpokes ++ " pokemon:" ]
                                    ++ if numpokes > 0
                                         then [ nick ++ ": " ++ intercalate ", " pokenames ]
                                         else []
                         _       -> mzero
            numpokes = length pokes
            pokenames = map (T.unpack . _speciesName) pokes
            outs = caughtmsg <> checkmsg <> dexmsg
        returnA -< outs
      Left _  ->
        returnA -< [ nick ++ ": Invalid command.  Welp." ]
    


