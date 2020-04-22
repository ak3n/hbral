module Data.HBRAList.Tree where

import Data.Kind
import Prelude hiding (lookup, Either(..))
import Data.HBRAList.Nat

data Tree (a :: Type) :: Nat -> Type where
  Leaf :: a -> Tree a Zero
  Node :: Tree a n -> Tree a n -> Tree a (Succ n)

instance Show a => Show (Tree a n) where
  show (Leaf a) = "Leaf " ++ show a
  show (Node t1 t2) = "Node (" ++ show t1 ++ ") (" ++ show t2 ++ ")"

data Path :: Nat -> Type where
  Here :: Path Zero
  Left :: Path n -> Path (Succ n)
  Right :: Path n -> Path (Succ n)

instance Show (Path n) where
  show Here = "Here"
  show (Left p) = "Left " ++ show p
  show (Right p) = "Right " ++ show p

lookup :: Tree a n -> Path n -> a
lookup (Node t1 t2) (Left p) = lookup t1 p
lookup (Node t1 t2) (Right p) = lookup t2 p
lookup (Leaf x) Here = x
