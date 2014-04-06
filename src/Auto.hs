{-# LANGUAGE BangPatterns #-}

module Auto where

-- much borrowed from http://lpaste.net/raw/101205

import Data.Binary
import Control.Category
import Data.Binary.Put
import Data.Binary.Get
import Data.ByteString.Lazy
import Control.Monad.Fix
import Control.Monad
import Control.Applicative
import Prelude hiding ((.), id)
import Control.Arrow

data Auto m a b = Auto { loadAuto :: Get (Auto m a b)
                       , saveAuto :: Put
                       , stepAuto :: a -> m (b, Auto m a b)
                       }

instance Monad m => Functor (Auto m a) where
    fmap f (Auto load save step) =
        Auto (fmap (fmap f) load)
             save
             (liftM (f *** fmap f) . step)

instance Monad m => Applicative (Auto m a) where
    pure x =
        let w = Auto (pure (pure x)) (pure ()) (\_ -> return (x, w))
        in  w

    Auto loadf savef stepf <*> Auto loadx savex stepx =
        Auto (liftA2 (<*>) loadf loadx) (savef *> savex) $ \x' ->
            liftM2 (\(f, wf) (x, wx) -> (f x, wf <*> wx))
                   (stepf x')
                   (stepx x')

instance Monad m => Category (Auto m) where
    id = Auto (pure id) (pure ()) (\x -> return (x, id))

    Auto load2 save2 step2 . Auto load1 save1 step1 =
        Auto (liftA2 (.) load2 load1) (save2 *> save1) $ \x0 -> do
            (x1, w1) <- step1 x0
            (x2, w2) <- step2 x1
            return (x2, w2 . w1)

instance Monad m => Arrow (Auto m) where
    arr f =
        let w = Auto (pure (arr f)) (pure ()) (\x -> return (f x, w))
        in  w

    first (Auto load save step) =
        Auto (fmap first load) save $ \(x', y) ->
            liftM (\(x, w) -> ((x, y), first w))
                  (step x')

instance MonadFix m => ArrowLoop (Auto m) where
    loop (Auto load save step) =
        Auto (fmap loop load) save $ \x' ->
            liftM (fst *** loop) .
            mfix $ \ ~((_, d), _) -> step (x', d)

instance Monad m => ArrowChoice (Auto m) where
    left a@(Auto load save step) =
        Auto (fmap left load) save $ \x' ->
          case x' of
            Left  y -> liftM (Left *** left) (step y)
            Right z -> return (Right z, left a)

encodeAuto :: Auto m a b -> ByteString
encodeAuto = runPut . saveAuto

decodeAuto :: Auto m a b -> ByteString -> Auto m a b
decodeAuto w = runGet (loadAuto w)

integral :: (Monad m, Num a, Binary a) => a -> Auto m a a
integral x' = Auto (fmap integral get) (put x') $ \dx ->
                let !x = x' + dx
                in  return (x, integral x)


-- integral :: (Fractional a, Monad m, Serialize a) => a -> Wire m a a
-- -- integral x' =
--     Wire (fmap integral get) (put x') $ \dt dx ->
--         let !x = x' + realToFrac dt*dx in
--         return (x, integral x)
