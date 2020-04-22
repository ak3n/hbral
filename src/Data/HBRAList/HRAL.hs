{-# LANGUAGE AllowAmbiguousTypes #-}
module Data.HBRAList.HRAL where

import Data.Kind
import Prelude hiding (lookup, Either(..))

import Data.HBRAList.Universe
import Data.HBRAList.Nat as N
import qualified Data.HBRAList.Bin as Bin
import qualified Data.HBRAList.RAL as R
import qualified Data.HBRAList.Tree as T
import qualified Data.HBRAList.HTree as HT

data HRAL (n :: Nat) (b :: Bin.Bin) :: R.RAL U n b -> Type where
  HNil :: HRAL n 'Bin.End R.Nil
  HCons1 :: HT.HTree n t -> HRAL ('Succ n) b ral -> HRAL n (Bin.One b) (R.Cons1 t ral)
  HCons0 :: HRAL ('Succ n) b ral -> HRAL n (Bin.Zero b) (R.Cons0 ral)

data HPos (n :: Nat) (b :: Bin.Bin) :: R.RAL U n b -> U -> Type where
  HHere :: HT.HPath n t u -> HPos n (Bin.One b) (R.Cons1 t ral) u
  HThere0 :: HPos (Succ n) b ral u -> HPos n (Bin.Zero b) (R.Cons0 ral) u
  HThere1 :: HPos (Succ n) b ral u -> HPos n (Bin.One b) (R.Cons1 t ral) u

lookup :: HRAL n b ral -> HPos n b ral u -> Val u
lookup (HCons1 t hral) (HHere path) = HT.lookup t path
lookup (HCons0 hral) (HThere0 p) = lookup hral p
lookup (HCons1 _ hral) (HThere1 p) = lookup hral p

consTree :: HT.HTree n t -> HRAL n b ral -> HRAL n (Bin.Succ b) (R.ConsTree t ral)
consTree t HNil = HCons1 t HNil
consTree t (HCons1 t' hral) = HCons0 (consTree (HT.HNode t t') hral)
consTree t (HCons0 hral) = HCons1 t hral

-- cons :: Val u -> HRAL Zero b ral -> HRAL 'Zero (Bin.Succ b) (R.ConsTree (T.Leaf u) ral)
-- cons x r = consTree (HT.HLeaf x) r