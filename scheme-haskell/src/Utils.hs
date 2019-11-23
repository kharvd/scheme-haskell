module Utils where

scanlM :: (Monad m) => (b -> a -> m b) -> m b -> [a] -> [m b]
scanlM f = scanl f'
    where f' mx y = mx >>= flip f y

recoverWith :: Maybe a -> Maybe a -> Maybe a
recoverWith m1@(Just x) _ = m1
recoverWith _ m2 = m2