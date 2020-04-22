module Data.HBRAList.Universe where

import Data.Kind


data Top = Top

data U :: Type where
  I :: U
  Arrow :: U -> U -> U

type family Val (u :: U) :: Type where
  Val 'I = Top
  Val ('Arrow u1 u2) = Val u1 -> Val u2
