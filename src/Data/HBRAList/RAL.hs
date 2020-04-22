{-# LANGUAGE UndecidableInstances #-}
module Data.HBRAList.RAL where

import Data.Kind
import Prelude hiding (lookup, Either(..))

import Data.HBRAList.Nat
import Data.HBRAList.Universe
import qualified Data.HBRAList.Tree as T
import qualified Data.HBRAList.Bin as Bin


data RAL (a :: Type) :: Nat -> Bin.Bin -> Type where
  Nil :: RAL a n Bin.End
  Cons1 :: T.Tree a n -> RAL a (Succ n) b -> RAL a n (Bin.One b)
  Cons0 :: RAL a (Succ n) b -> RAL a n (Bin.Zero b)

instance Show a => Show (RAL a n b) where
  show Nil = "Nil"
  show (Cons1 t ral) = "Cons1 (" ++ show t ++ ") (" ++ show ral ++ ")"
  show (Cons0 ral) = "Cons0 (" ++ show ral ++ ")"

data Pos :: Nat -> Bin.Bin -> Type where
  Here :: T.Path n -> Pos n (Bin.One b)
  There0 :: Pos (Succ n) b -> Pos n (Bin.Zero b)
  There1 :: Pos (Succ n) b -> Pos n (Bin.One b)

instance Show (Pos n b) where
  show (Here p) = "Here " ++ show p
  show (There0 p) = "There0 " ++ show p
  show (There1 p) = "There1 " ++ show p

lookup :: RAL a n b -> Pos n b -> a
lookup (Cons1 t ral) (Here path) = T.lookup t path
lookup (Cons0 ral) (There0 i) = lookup ral i
lookup (Cons1 _ ral) (There1 i) = lookup ral i

consTree :: T.Tree a n -> RAL a n b -> RAL a n (Bin.Succ b)
consTree t Nil = Cons1 t Nil
consTree t (Cons1 t' r) = Cons0 (consTree (T.Node t t') r)
consTree t (Cons0 r) = Cons1 t r

cons :: a -> RAL a Zero b -> RAL a Zero (Bin.Succ b)
cons x r = consTree (T.Leaf x) r

type family ConsTree (t :: T.Tree a n) (ral :: RAL a n b) :: (RAL a n (Bin.Succ b)) where
  ConsTree t Nil = Cons1 t Nil
  ConsTree t (Cons1 t' r) = Cons0 (ConsTree (T.Node t t') r)
  ConsTree t (Cons0 r) = Cons1 t r

-- requires UndecidableInstances
type family Cons a (ral :: RAL U Zero b) :: (RAL U Zero (Bin.Succ b)) where
  Cons x r = ConsTree (T.Leaf x) r
