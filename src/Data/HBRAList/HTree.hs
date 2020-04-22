module Data.HBRAList.HTree where

import Data.Kind
import Prelude hiding (lookup, Either(..))

import Data.HBRAList.Universe
import Data.HBRAList.Nat as N
import qualified Data.HBRAList.Bin as Bin
import qualified Data.HBRAList.RAL as R
import qualified Data.HBRAList.Tree as T


data HTree (n :: Nat) :: T.Tree U (n :: Nat) -> Type where
  HLeaf :: Val u -> HTree 'Zero (T.Leaf u)
  HNode :: HTree n us -> HTree n vs -> HTree ('Succ n) (T.Node us vs)

data HPath (n :: Nat) :: T.Tree U n -> U -> Type where
  HHere :: HPath 'Zero (T.Leaf u) u
  HLeft :: HPath n us u -> HPath ('Succ n) (T.Node us vs) u
  HRight :: HPath n vs u -> HPath ('Succ n) (T.Node us vs) u

lookup :: HTree n us -> HPath n us u -> Val u
lookup (HNode t1 t2) (HLeft p) = lookup t1 p
lookup (HNode t1 t2) (HRight p) = lookup t2 p
lookup (HLeaf x) HHere = x
