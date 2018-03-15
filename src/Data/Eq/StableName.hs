-- {-# language CPP #-}
{-# language BangPatterns #-}
-- {-# language FlexibleInstances #-}
-- {-# language UndecidableInstances #-}

module Data.Eq.StableName
    ( StableNameEq, ($==), (==$)

    , StableNameEq1(..), StableNameEq2(..)
    ) where

import System.Mem.StableName

($==) :: (StableNameEq a) => a -> a -> IO Bool
($==) !x !y = do
    sx <- makeStableName x
    sy <- makeStableName y
    if sx == sy
      then pure True
      else x ==$ y

class StableNameEq a where
    (==$) :: a -> a -> IO Bool

-- -- | Fall-back instance which uses '=='.
-- instance {-# OVERLAPPABLE #-} (Eq a) => StableNameEq a where
--     x ==$ y = pure $ x == y

class StableNameEq1 f where
    liftStableNameEq
        :: (a -> b -> IO Bool)
        -> f a -> f b -> IO Bool

class StableNameEq2 f where
    liftStableNameEq2
        :: (a -> b -> IO Bool)
        -> (c -> d -> IO Bool)
        -> f a c -> f b d -> IO Bool
