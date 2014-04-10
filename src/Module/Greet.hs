{-# LANGUAGE Arrows #-}
{-# LANGUAGE TupleSections #-}

module Module.Greet (greetAuto) where

-- import Data.Map.Strict        (Map)
-- import Event
import Auto
import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.IO.Class
import Data.Binary
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Time
import System.Random
import Types
import qualified Data.Map.Strict as M

instance Binary UTCTime where
    get = read <$> get
    put = put . show


greetAuto :: MonadIO m => Interact m
greetAuto = proc (InMessage nick msg) -> do
    let isGreet = "jlebot" `isInfixOf` msg && hasGreeting msg
        reset   = do
          guard ("@greet reset" `isPrefixOf` msg)
          Just . fromMaybe nick . listToMaybe . drop 2 . words $ msg
    case () of
      _ | isGreet || isJust reset -> do
            t      <- time    -< ()
            gen    <- randGen -< ()

            let comm = maybe (Right (nick, t)) Left reset

            greets <- scanA processGreet M.empty -< comm

            let numg   = length $ M.findWithDefault [] nick greets
                gs     = greetings nick !! min (numg `div` 3) 3
                gind   = fst (randomR (0, length gs - 1) gen)

            returnA -< return $ case comm of
                         Right _ -> gs !! gind
                         Left n  -> concat [ "I have forgotten about "
                                           , n
                                           , " and our relationship."]

        | otherwise ->
            returnA -< mzero
  where
    processGreet m (Right (n,t)) = M.insertWith (addLimit 25) n [t] m
    processGreet m (Left n)      = M.insert n [] m
    addLimit n x xs = takeRec $ take n (x <> xs)
      where
        takeRec = case x of
                    []  -> id
                    y:_ -> takeWhile (\t -> (diffUTCTime y t) < 60*60*24)


hasGreeting :: String -> Bool
hasGreeting str = any (`elem` strwords )
                      ["hi", "hello", "hey", "sup", "hola", "oy", "yo"]
  where
    strwords = words str

greetings :: String -> [[String]]
greetings n = (map . map) f glist
  where
    f str = let (x,y) = break (== '%') str
            in  x ++ n ++ drop 1 y
    glist = [ [ "hi %"
              , "sup %"
              , "ahoy %"
              , "hi %!"
              , "yo %"
              , "hello %"
              , "greetings %"
              , "howdy do %"
              , "hey %"
              , "hey % :)"
              , "o/ %"
              ]
            , [ "hi again %"
              , "hey, how are you doing %?"
              , "whats new %"
              , "hey % pal"
              , "hey % buddy"
              , "%, sup bro"
              , "%, my man/woman"
              , "% how u!"
              , "% hows it hangin"
              , "%, lookin good"
              ]
            , [ "hey, %, my bestie"
              , "i love you %"
              , "how is my bff %"
              , "when are we going to hang, %?"
              , "% i miss you dearly"
              , "how would you touch me, %?"
              , "% you are the light of my world"
              , "%, missed you so much!"
              , "% you bring out the best in me"
              , "oh % <3"
              , "so nice to see your lovely face again %"
              ]
            , [ "please stop calling me %"
              , "stop bothering me %"
              , "i'm going to call the cops %"
              , "stop please %"
              , "% you used to be cool"
              , "okay % you're just getting creepy now"
              , "i'm pretty sure i got a restraining order on you %"
              , "i hate you %"
              , "words cannot describe how much you annoy me %"
              , "%, you're so annoying"
              , "%, we're over"
              , "%, we're through."
              , "% can we just move on please..."
              ]
            ]


time :: MonadIO m => Auto m a UTCTime
time = Auto (pure time) (put ()) $ \_ ->
    liftM (,time) (liftIO getCurrentTime)

randGen :: MonadIO m => Auto m a StdGen
randGen = Auto (pure randGen) (put ()) $ \_ ->
    liftM (,randGen) (liftIO newStdGen)

