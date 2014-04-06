{-# LANGUAGE Arrows #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import qualified Data.ByteString as B
import Control.Applicative
import Control.Arrow
import Control.Category
import Control.Exception
import Control.Monad
import Control.Monad.Fix
import Data.ByteString (ByteString)
import Data.Functor.Identity
import Data.Serialize
import Prelude hiding ((.), id)


type Time = Double


data Wire m a b =
    Wire {
      loadWire :: Get (Wire m a b),
      saveWire :: Put,
      stepWire :: Time -> a -> m (b, Wire m a b)
    }

instance (Monad m) => Applicative (Wire m a) where
    pure x =
        let w = Wire (pure (pure x)) (pure ()) (\_ _ -> return (x, w))
        in w

    Wire loadf savef stepf <*> Wire loadx savex stepx =
        Wire (liftA2 (<*>) loadf loadx) (savef *> savex) $ \dt x' ->
            liftM2 (\(f, wf) (x, wx) -> (f x, wf <*> wx))
                   (stepf dt x')
                   (stepx dt x')

instance (Monad m) => Arrow (Wire m) where
    arr f =
        let w = Wire (pure (arr f)) (pure ()) (\_ x -> return (f x, w))
        in w

    first (Wire load save step) =
        Wire (fmap first load) save $ \dt (x', y) ->
            liftM (\(x, w) -> ((x, y), first w))
                  (step dt x')

instance (MonadFix m) => ArrowLoop (Wire m) where
    loop (Wire load save step) =
        Wire (fmap loop load) save $ \dt x' ->
            liftM (fst *** loop) .
            mfix $ \ ~((_, d), _) -> step dt (x', d)

instance (Monad m) => Category (Wire m) where
    id = Wire (pure id) (pure ()) (\_ x -> return (x, id))

    Wire load2 save2 step2 . Wire load1 save1 step1 =
        Wire (liftA2 (.) load2 load1) (save2 *> save1) $ \dt x0 -> do
            (x1, w1) <- step1 dt x0
            (x2, w2) <- step2 dt x1
            return (x2, w2 . w1)

instance (Monad m) => Functor (Wire m a) where
    fmap f (Wire load save step) =
        Wire (fmap (fmap f) load) save $ \dt ->
            liftM (f *** fmap f) . step dt


integral :: (Fractional a, Monad m, Serialize a) => a -> Wire m a a
integral x' =
    Wire (fmap integral get) (put x') $ \dt dx ->
        let !x = x' + realToFrac dt*dx in
        return (x, integral x)


timeFrom :: (Monad m) => Time -> Wire m a Time
timeFrom t' =
    Wire (fmap timeFrom get) (put t') $ \dt _ ->
        let !t = t' + dt in
        return (t, timeFrom t)


encodeWire :: Wire m a b -> ByteString
encodeWire = runPut . saveWire


decodeWire :: Wire m a b -> ByteString -> Either String (Wire m a b)
decodeWire w = runGet (loadWire w)


myWire :: Wire Identity a Double
myWire =
    proc _ -> do
        rec vel <- integral 0.1 -< vel
        integral 8 -< vel


main :: IO ()
main = do
    putStrLn "Saving wire..."
    B.writeFile "test.sav" (encodeWire myWire)

    mw <- fmap (decodeWire myWire) (B.readFile "test.sav")
    case mw of
      Left err -> throwIO (userError $ "Unable to load wire: " ++ err)
      Right w  -> putStrLn "Successfully loaded wire!"
