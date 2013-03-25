{-# LANGUAGE DeriveDataTypeable #-}
module ReprTree (ReprTree, reprTreeString, reprTree) where

import Data.Tree
import Data.Generics
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

type ReprTree = Tree String

reprTreeString :: (Data a) => a -> String
reprTreeString = unlines . draw . reprTree
  where
    draw :: ReprTree -> [String]
    draw (Node x ts0) = x : drawSubTrees ts0
      where
        drawSubTrees [] = []
        drawSubTrees [t] =
            shift "- " "  " (draw t)
        drawSubTrees (t:ts) =
            shift "- " "| " (draw t) ++ drawSubTrees ts
        shift first other = zipWith (++) (first : repeat other)

reprTree :: Data a => a -> ReprTree
reprTree = adtReprTree 
  `ext2Q` mapReprTree 
  `ext2Q` pairReprTree 
  `ext1Q` listReprTree 
  `ext1Q` setReprTree 
  `extQ` textReprTree 
  `extQ` stringReprTree

textReprTree :: Text -> ReprTree
textReprTree x = Node (Text.unpack x) []

stringReprTree :: String -> ReprTree
stringReprTree x = Node x []

adtReprTree :: Data a => a -> ReprTree
adtReprTree a = Node (showConstr (toConstr a)) (gmapQ reprTree a)

mapReprTree :: (Data a, Data k) => Map k a -> ReprTree
mapReprTree = Node "Map" . map pairReprTree . Map.toList where

pairReprTree :: (Data a, Data b) => (a, b) -> ReprTree
pairReprTree (a, b) = Node "," [reprTree a, reprTree b]

listReprTree :: (Data a) => [a] -> ReprTree
listReprTree = Node ":" . map reprTree

setReprTree :: (Data a) => Set a -> ReprTree
setReprTree = Node "Set" . map reprTree . Set.toList
