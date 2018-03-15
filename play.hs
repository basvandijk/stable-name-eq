{-# language TemplateHaskell #-}

module Main where

import Data.Eq.StableName.TH
import Data.Eq.StableName


data MyRec
   = MyRec
     { field1 :: !Bool
     , field2 :: !(Maybe MyRec)
     , field3 :: ![Int]
     }

deriveStableNameEq ''MyRec

myRec1 :: MyRec
myRec1 = MyRec{ field1 = False
              , field2 = Nothing
              , field3 = [1,2,3]
              }

myRec2 :: MyRec
myRec2 = MyRec{ field1 = False
              , field2 = Nothing
              , field3 = [1,2,4]
              }

main :: IO ()
main = print $ myRec1 $== myRec2
