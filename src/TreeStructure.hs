{-# LANGUAGE DeriveDataTypeable #-}
module TreeStructure (treeRepr, tree) where

import Data.Tree
import Data.Generics
import Data.String
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set


treeRepr :: (Data a, IsString b) => a -> b
treeRepr = fromString . unlines . draw . tree
  where
    draw :: Tree String -> [String]
    draw (Node x ts0) = x : drawSubTrees ts0
      where
        drawSubTrees [] = []
        drawSubTrees [t] =
            shift "- " "  " (draw t)
        drawSubTrees (t:ts) =
            shift "- " "| " (draw t) ++ drawSubTrees ts
        shift first other = zipWith (++) (first : repeat other)

tree :: Data a => a -> Tree String
tree = adtTree 
  `ext2Q` mapDataTree 
  `ext2Q` pairDataTree 
  `ext1Q` listDataTree 
  `ext1Q` setDataTree 
  `extQ` textTree 
  `extQ` stringTree

textTree :: Text -> Tree String
textTree x = Node (Text.unpack x) []

stringTree :: String -> Tree String
stringTree x = Node x []

adtTree :: Data a => a -> Tree String
adtTree a = Node (showConstr (toConstr a)) (gmapQ tree a)

mapDataTree :: (Data a, Data k) => Map k a -> Tree String
mapDataTree = Node "Map" . map pairDataTree . Map.toList where

pairDataTree :: (Data a, Data b) => (a, b) -> Tree String
pairDataTree (a, b) = Node "," [tree a, tree b]

listDataTree :: (Data a) => [a] -> Tree String
listDataTree = Node ":" . map tree

setDataTree :: (Data a) => Set a -> Tree String
setDataTree = Node "Set" . map tree . Set.toList
