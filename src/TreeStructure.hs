{-# LANGUAGE DeriveDataTypeable #-}
module TreeStructure (treeRepr, tree) where

import Control.Applicative
import Data.Tree
import Data.Generics
import Data.String
import Data.Text (Text)
import qualified Data.Text as Text


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
tree = fix . tree' 
  where
    fix (Node name forest)
      | name == "(:)" 
      , a : b : [] <- forest
        = Node ":" $ (fix a) : (subForest $ fix b)
      | name == "(,)" = Node "," $ fix <$> forest
      | otherwise = Node name $ fix <$> forest

tree' :: Data a => a -> Tree String
tree' = adtTree `extQ` textTree `extQ` stringTree

textTree :: Text -> Tree String
textTree x = Node (Text.unpack x) []

stringTree :: String -> Tree String
stringTree x = Node x []

adtTree :: Data a => a -> Tree String
adtTree a = Node (showConstr (toConstr a)) (gmapQ tree' a)

