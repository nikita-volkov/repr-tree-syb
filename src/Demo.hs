{-# LANGUAGE DeriveDataTypeable #-}

import Control.Applicative
import Data.Tree
import Data.Generics
import Data.String
import TreeStructure

data SomeType = A [String] Int | B | C Int | D [[String]] 
  deriving (Typeable, Data)

xxx = A ["a", "b", "c"] 9 
    : C 3 
    : B 
    : D [["asdf", "123", "ldskfjkl"], ["f"]]
    : []

main = do
  putStrLn $ treeRepr xxx
