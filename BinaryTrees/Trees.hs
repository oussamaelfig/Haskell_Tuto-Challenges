module BinaryTree
  ( BinaryTree
  , valuesPreOrder
  , valuesInOrder
  , valuesPostOrder
  , leaves
  , deepestL
  , deepestR
  , rightMostLeaf
  , leftMostLeaf
  , leavesWithLevel
  , minmax
  ) where

import Data.Function
import Data.List

data BinaryTree a
  = Empty
  | Leaf a
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Show, Eq)

instance Functor BinaryTree where
  fmap = treeMap

instance Foldable BinaryTree where
  foldr = treeFoldr

treeMap :: (a -> b) -> BinaryTree a -> BinaryTree b
treeMap f (Leaf a) = Leaf (f a)
treeMap f (Node leftSubTree a rightSubTree) =
  Node (treeMap f leftSubTree) (f a) (treeMap f rightSubTree)
treeMap _ _ = Empty

valuesPreOrder :: BinaryTree a -> [a]
valuesPreOrder (Leaf a) = [a]
valuesPreOrder (Node leftSubTree a rightSubTree) =
  [a] ++ valuesPreOrder leftSubTree ++ valuesPreOrder rightSubTree
valuesPreOrder _ = []

valuesInOrder :: BinaryTree a -> [a]
valuesInOrder (Leaf a) = [a]
valuesInOrder (Node leftSubTree a rightSubTree) =
  valuesInOrder leftSubTree ++ [a] ++ valuesInOrder rightSubTree
valuesInOrder _ = []

valuesPostOrder :: BinaryTree a -> [a]
valuesPostOrder (Leaf a) = [a]
valuesPostOrder (Node leftSubTree a rightSubTree) =
  valuesPostOrder leftSubTree ++ valuesPostOrder rightSubTree ++ [a]
valuesPostOrder _ = []

treeFoldr :: (a -> b -> b) -> b -> BinaryTree a -> b
treeFoldr f z Empty = z
treeFoldr f z (Leaf a) = f a z
treeFoldr f z (Node leftSubTree a rightSubTree) =
  treeFoldr f (f a (treeFoldr f z rightSubTree)) leftSubTree

leaves :: BinaryTree a -> [a]
leaves (Leaf a) = [a]
leaves (Node leftSubTree a rightSubTree) =
  case (leftSubTree, rightSubTree) of
    (_, Empty) -> leaves leftSubTree
    (Empty, _) -> leaves rightSubTree
    otherwise -> leaves leftSubTree ++ leaves rightSubTree
leaves _ = []

leavesWithLevel :: Int -> BinaryTree a -> [(a, Int)]
leavesWithLevel level (Leaf a) = [(a, level)]
leavesWithLevel level (Node leftSubTree a rightSubTree) =
  case (leftSubTree, rightSubTree) of
    (_, Empty) -> leavesWithSuccLevel leftSubTree
    (Empty, _) -> leavesWithSuccLevel rightSubTree
    otherwise ->
      leavesWithSuccLevel leftSubTree ++ leavesWithSuccLevel rightSubTree
  where
    leavesWithSuccLevel = leavesWithLevel (succ level)
leavesWithLevel _ _ = []

deepestR :: BinaryTree a -> Maybe a
deepestR Empty = Nothing
deepestR bt = Just . fst . last . sortOn snd . (leavesWithLevel 0) $ bt

deepestL :: BinaryTree a -> Maybe a
deepestL Empty = Nothing
deepestL bt =
  Just . fst . maximumBy sndCompare . reverse . sortOn snd . (leavesWithLevel 0) $
  bt
  where
    sndCompare = compare `on` snd

rightMostLeaf :: BinaryTree a -> Maybe a
rightMostLeaf Empty = Nothing
rightMostLeaf bt = Just . last . leaves $ bt

leftMostLeaf :: BinaryTree a -> Maybe a
leftMostLeaf Empty = Nothing
leftMostLeaf bt = Just . head . leaves $ bt

minmax :: Ord a => BinaryTree a -> (a, a)
minmax bt = (minimum $ values bt, maximum $ values bt)
  where
    values (Leaf a) = [a]
    values (Node leftSubTree a rightSubTree) =
      case (leftSubTree, rightSubTree) of
        (_, Empty) -> a : values leftSubTree
        (Empty, _) -> a : values rightSubTree
        otherwise -> values leftSubTree ++ values rightSubTree
    values _ = []