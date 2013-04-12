{-# LANGUAGE DeriveDataTypeable #-}
module ReprTree (reprTree, reprTreeString) where

import Data.Tree
import Data.Generics
import Data.List
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set


-- | A data representation in form of a formatted multiline string, such as
-- the following:
-- 
-- @
-- :
-- - A
-- | - :
-- | | - a
-- | | - b
-- | | - c
-- | - 9
-- - C
-- | - 3
-- - B
-- - D
--   - :
--     - :
--     | - asdf
--     | - 123
--     | - ldskfjkl
--     - :
--       - f
-- @
-- 
-- Which is a result of running the following code:
-- 
-- > import Data.Generics (Data, Typeable)
-- >
-- > data SomeType = 
-- >   A [String] Int | 
-- >   B | 
-- >   C Int | 
-- >   D [[String]]
-- >   deriving (Typeable, Data)
-- > 
-- > xxx = A ["a", "b", "c"] 9 
-- >     : C 3 
-- >     : B 
-- >     : D [["asdf", "123", "ldskfjkl"], ["f"]]
-- >     : []
-- > 
-- > main = putStrLn $ reprTreeString xxx
-- 
reprTreeString :: (Data a) => a -> String
reprTreeString = unlines . treeLines . reprTree where
  treeLines (Node x ts) = x : subTreesLines ts
  subTreesLines [] = []
  subTreesLines [t] = shift "- " "  " (treeLines t)
  subTreesLines (t:ts) = shift "- " "| " (treeLines t) ++ subTreesLines ts
  shift first other = zipWith (++) (first : repeat other)

-- | Get a representation tree of a generic data structure using SYB. Can be 
-- used to implement a custom converter to textual representation.
reprTree :: Data a => a -> Tree String
reprTree = adtReprTree 
  `ext2Q` mapReprTree 
  `ext2Q` pairReprTree 
  `ext1Q` listReprTree 
  `ext1Q` setReprTree 
  `extQ` textReprTree 
  `extQ` stringReprTree

textReprTree :: Text -> Tree String
textReprTree x = Node (Text.unpack x) []

stringReprTree :: String -> Tree String
stringReprTree x = Node x []

adtReprTree :: Data a => a -> Tree String
adtReprTree a = Node (stripBraces $ showConstr $ toConstr a) (gmapQ reprTree a) 
  where
    stripBraces :: String -> String
    stripBraces s = 
      fromMaybe s $ 
        stripPrefix "(" s >>= fmap reverse . stripPrefix ")" . reverse

mapReprTree :: (Data a, Data k) => Map k a -> Tree String
mapReprTree = Node "Map" . map pairReprTree . Map.toList

pairReprTree :: (Data a, Data b) => (a, b) -> Tree String
pairReprTree (a, b) = Node "," [reprTree a, reprTree b]

listReprTree :: (Data a) => [a] -> Tree String
listReprTree = Node ":" . map reprTree

setReprTree :: (Data a) => Set a -> Tree String
setReprTree = Node "Set" . map reprTree . Set.toList
