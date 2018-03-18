{-# language BangPatterns #-}

-- {-# language FlexibleInstances #-}
-- {-# language UndecidableInstances #-}

module Data.Eq.StableName.Class
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

-- TODO: How can we solve the following warning when enabling the catch-all instance:
--
-- warning: [-Wsimplifiable-class-constraints]
--   • The constraint ‘StableNameEq a’ matches an instance declaration
--     instance [overlappable] Eq a => StableNameEq a
--       -- Defined at src/Data/Eq/StableName/Class.hs:27:31
--     This makes type inference for inner bindings fragile;
--       either use MonoLocalBinds, or simplify it using the instance
--
-- -- | Fall-back instance which uses '=='.
-- instance {-# OVERLAPPABLE #-} (Eq a) => StableNameEq a where
--     x ==$ y = pure $ x == y

class StableNameEq1 f where
    liftStableNameEq
        ::  (a ->   b -> IO Bool)
        -> f a -> f b -> IO Bool

class StableNameEq2 f where
    liftStableNameEq2
        :: ( a   ->   b   -> IO Bool)
        -> (   c ->     d -> IO Bool)
        -> f a c -> f b d -> IO Bool
