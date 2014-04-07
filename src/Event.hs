module Event where

import Auto
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put

hold :: (Monad m, Binary a) => a -> Auto m (Maybe a) a
hold x' = Auto (fmap hold get) (put x') $ \dx -> do
            case dx of
              Nothing -> return (x', hold x')
              Just x  -> return (x, hold x)

scanE :: (Monad m, Binary b) => (b -> a -> b) -> b -> Auto m (Maybe a) b
scanE f x' = Auto (fmap (scanE f) get) (put x') $ \dx -> do
               case dx of
                 Nothing -> return (x', scanE f x')
                 Just x  -> let y = f x' x
                            in  return (y, scanE f y)
