module Stadium.TL where

import Prelude

import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), NoArguments(..), Sum(..), to)
import Data.Map.Internal (unsafeBalancedNode)
import Heterogeneous.Mapping (class HMap, class Mapping, hmap)
import Prim.Row as Row
import Record as Record
import Type.Data.Symbol (class IsSymbol)
import Type.Prelude (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

class MkConstructors :: Type -> Row Type -> Constraint
class MkConstructors a r | a -> r where
  mkConstructors :: Record r

instance main ::
  ( Generic a rep
  , MkConstructorsRep rep r'
  , HMap (FnMapFromRep a) (Record r') (Record r)
  ) =>
  MkConstructors a r where
  mkConstructors = ret
    where
    ret :: Record r
    ret = hmap (FnMapFromRep @a) r'

    r' :: Record r'
    r' = mkConstructorsRep @rep

---

class MkConstructorsRep :: Type -> Row Type -> Constraint
class MkConstructorsRep rep r | rep -> r where
  mkConstructorsRep :: Record r

instance main2 ::
  ( MkConstructorsRep lhs lhsr'
  , MkConstructorsRep rhs rhsr'
  , HMap FnMapLeft (Record lhsr') (Record lhsr)
  , HMap FnMapRight (Record rhsr') (Record rhsr)
  , Row.Union lhsr rhsr r
  ) =>
  MkConstructorsRep (Sum lhs rhs) r where
  mkConstructorsRep = ret
    where
    lhsr' :: Record lhsr'
    lhsr' = mkConstructorsRep @lhs

    lhsr :: Record lhsr
    lhsr = hmap FnMapLeft lhsr'

    rhsr' :: Record rhsr'
    rhsr' = mkConstructorsRep @rhs

    rhsr :: Record rhsr
    rhsr = hmap FnMapRight rhsr'

    ret :: Record r
    ret = Record.union lhsr rhsr

data FnMapLeft = FnMapLeft

instance main3 :: Mapping FnMapLeft (args -> a) (args -> Sum a b) where
  mapping _ x = Inl <$> x

else instance main34 :: Mapping FnMapLeft a (Sum a b) where
  mapping _ x = Inl x

data FnMapRight = FnMapRight

instance main35 :: Mapping FnMapRight (args -> b) (args -> Sum a b) where
  mapping _ x = Inr <$> x

else instance main36 :: Mapping FnMapRight b (Sum a b) where
  mapping _ x = Inr x

data FnMapFromRep a = FnMapFromRep

instance main55 :: (Generic a rep) => Mapping (FnMapFromRep a) (args -> rep) (args -> a) where
  mapping _ x = to <<< x

else instance main5 :: (Generic a rep) => Mapping (FnMapFromRep a) rep a where
  mapping _ x = to x

instance main6 ::
  ( Row.Cons sym (Constructor sym NoArguments) () r
  , IsSymbol sym
  ) =>
  MkConstructorsRep (Constructor sym NoArguments) r where
  mkConstructorsRep = ret
    where
    val :: Constructor sym NoArguments
    val = Constructor NoArguments

    ret :: Record r
    ret = Record.insert (Proxy :: _ sym) val {}

else instance
  ( Row.Cons sym (args -> Constructor sym (Argument args)) () r
  , IsSymbol sym
  ) =>
  MkConstructorsRep (Constructor sym (Argument args)) r where
  mkConstructorsRep = ret
    where
    fn :: args -> Constructor sym (Argument args)
    fn = Constructor <<< Argument

    ret :: Record r
    ret = Record.insert (Proxy :: _ sym) fn {}