module Event where

import Auto
import Data.Binary
import Control.Applicative

type Event = Maybe

noEvent :: Maybe a
noEvent = Nothing

event :: a -> Maybe a
event = Just

whenE :: Bool -> a -> Maybe a
whenE False _ = Nothing
whenE True a  = Just a


hold' :: (Monad m, Binary a) => a -> Auto m (Event a) a
hold' x' = Auto (fmap hold' get) (put x') $ \dx ->
             case dx of
               Nothing -> return (x', hold' x')
               Just x  -> return (x, hold' x)
           -- or scanE (flip const)

hold :: (Monad m, Binary a) => Auto m (Event a) (Maybe a)
hold = go Nothing
  where
    go x' = Auto (fmap go get) (put x') $ \dx ->
              case dx of
                Nothing -> return (x', go x')
                Just _  -> return (dx, go dx)

scanE :: (Monad m, Binary b) => (b -> a -> b) -> b -> Auto m (Event a) b
scanE f x' = Auto (fmap (scanE f) get) (put x') $ \dx ->
               case dx of
                 Nothing -> return (x', scanE f x')
                 Just x  -> let y = f x' x
                            in  return (y, scanE f y)

switch :: Monad m => Auto m a (b, Event (Auto m a b)) -> Auto m a b
switch a' = Auto (fmap switch (loadAuto a')) (saveAuto a') $ \dx -> do
              ((out,sw),a) <- stepAuto a' dx
              case sw of
                Nothing -> return (out, switch a)
                Just a2 -> return (out, a2)
                -- Just a2 -> stepAuto a2 dx


until :: Monad m => Auto m (a, Event b) (Maybe a)
until = Auto (pure Event.until) (pure ()) $ \(x,e) ->
          case e of
            Just _  -> return (Nothing, pure Nothing)
            Nothing -> return (Just x, Event.until)

(-->) :: Monad m => Auto m a (Maybe b) -> Auto m a (Maybe b) -> Auto m a (Maybe b)
a0 --> a1 = Auto ((-->) <$> loadAuto a0 <*> loadAuto a1)
                 (saveAuto a0 >> saveAuto a1)
                 $ \x -> do
              (res, a0') <- stepAuto a0 x
              case res of
                Just _  -> return (res, a0' --> a1)
                Nothing -> do
                  (res', a1') <- stepAuto a1 x
                  return (res', a1')

accelAuto :: Monad m => Auto m (Event a) b -> Auto m (Event [a]) b
accelAuto a0 = Auto (accelAuto <$> loadAuto a0) (saveAuto a0) $ flip go a0
  where
    go (Just (x:xs)) a' = do
      (_, a) <- stepAuto a' (Just x)
      go (Just xs) a
    go _ a' = do
      (res, a) <- stepAuto a' Nothing
      return (res, accelAuto a)


-- (-->) :: Monad m => Auto m a (Maybe b) -> Auto m a b -> Auto m a b
-- a0 --> a1 = Auto ((-->) <$> get <*> get) (put a0 >> put a1) $ \x -> do
--               (res, a0') <- stepAuto a0 x
--               case res of
--                 Just x' -> return (x', a0' --> a1)
--                 Nothing -> do
--                   (res', a1') <- stepAuto a1 x
--                   return (res', a1')

infixr 1 -->
