{-# language TemplateHaskell #-}

module Main where

import Criterion.Main
import Data.Eq.StableName

import Data.Ratio
import Data.Fixed
import Data.Version
import Data.Semigroup

data MyType = MkMyType
    { f01 :: !(Maybe Int)
    , f02 :: ![Bool]
    , f03 :: !Bool
    -- , f04 :: !(Either String Double)
    -- , f05 :: !(Bool, Float)
    -- , f06 :: !(Ratio Integer)
    -- , f07 :: !Version
    -- , f08 :: !(Max Pico)
    , f09 :: !Integer
    , f10 :: !(Maybe MyType)

    -- , f11 :: !(Maybe Int)
    -- , f12 :: ![Bool]
    -- , f13 :: !Bool
    -- , f14 :: !(Either String Double)
    -- , f15 :: !(Bool, Float)
    -- , f16 :: !(Ratio Integer)
    -- , f17 :: !Version
    -- , f18 :: !(Max Pico)
    -- , f19 :: !Integer
    -- , f20 :: !(Maybe MyType)
    } deriving (Eq)

deriveStableNameEq ''MyType

val1 :: MyType
val1 = MkMyType
    { f01 = Just 3
    , f02 = [True, False, False, True, False, True, True]
    , f03 = True
    -- , f04 = Left "Hello World!"
    -- , f05 = (False, pi)
    -- , f06 = 123456789 % 987654321
    -- , f07 = Version [1,3,0] ["r2"]
    -- , f08 = Max (1 / 100000000000) <> Max (1 / 10000000000)
    , f09 = 1234567898765432123456789
    , f10 = Just val1{f10 = Nothing}

    -- , f11 = Just 3
    -- , f12 = take 100 $ cycle [True, False]
    -- , f13 = True
    -- , f14 = Left "Hello World!"
    -- , f15 = (False, pi)
    -- , f16 = 123456789 % 987654321
    -- , f17 = Version [1,3,0] ["r2"]
    -- , f18 = Max (1 / 100000000000) <> Max (1 / 10000000000)
    -- , f19 = 1234567898765432123456789
    -- , f20 = Just val1{f10 = Nothing}
    }

val2 :: MyType
val2 = val1
       { f10 = Nothing
       }

main :: IO ()
main = (val1 ==$ val2) >>= print

-- main :: IO ()
-- main = defaultMain
--        [ bench "=="  (nf (uncurry (==)) (val1, val2))
--        -- , bench "$==" (nfIO (val1 $== val2))
--        , bench "==$" (nfIO (val1 ==$ val2))
--        ]
