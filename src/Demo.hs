{-# LANGUAGE DeriveDataTypeable #-}

import Control.Applicative
import Data.Tree
import Data.Generics
import Data.String
import Data.Map as Map
import Data.Set as Set
import TreeStructure

data SomeType = 
  A [String] Int | 
  B | 
  C Int | 
  D [[String]] | 
  E (Map SomeType Int) |
  F (String, Int) |
  G (Set Int)
  deriving (Typeable, Data, Ord, Eq)

xxx = A ["a", "b", "c"] 9 
    : C 3 
    : B 
    : D [["asdf", "123", "ldskfjkl"], ["f"]]
    : E (Map.fromList [(B, 3), (C 2, 4)])
    : F ("asdf", 23)
    : G (Set.fromList [1,2,2,3])
    : []

main = do
  putStrLn $ treeRepr xxx
