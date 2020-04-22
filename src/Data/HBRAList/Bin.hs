module Data.HBRAList.Bin where

import Data.Kind
import Prelude hiding (succ)

data Bin :: Type where
  End :: Bin
  One :: Bin -> Bin
  Zero :: Bin -> Bin

instance Show Bin where
  show End = "End"
  show (One b) = "One " ++ show b
  show (Zero b) = "Zero " ++ show b

succ :: Bin -> Bin
succ End = One End
succ (One b) = Zero (succ b)
succ (Zero b) = One b

type family Succ (b :: Bin) :: Bin where
  Succ 'End = 'One 'End
  Succ ('One b) = 'Zero (Succ b)
  Succ ('Zero b) = 'One b