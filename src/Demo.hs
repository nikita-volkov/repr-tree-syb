{-# LANGUAGE DeriveDataTypeable #-}

import Control.Applicative
import Data.Tree
import Data.Generics
import Data.String
import Data.Map
import TreeStructure

data SomeType = A [String] Int | B | C Int | D [[String]] | E (Map SomeType Int)
  deriving (Typeable, Data, Ord, Eq)

xxx = A ["a", "b", "c"] 9 
    : C 3 
    : B 
    : D [["asdf", "123", "ldskfjkl"], ["f"]]
    : E (fromList [(B, 3), (C 2, 4)])
    : []

main = do
  putStrLn $ treeRepr xxx
