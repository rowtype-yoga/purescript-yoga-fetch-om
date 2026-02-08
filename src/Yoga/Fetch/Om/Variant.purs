module Yoga.Fetch.Om.Variant
  ( class OnlyVariant
  , onlyImpl
  , only
  , class VariantOrValue
  , variantOrValue
  ) where

import Prelude

import Data.Symbol (class IsSymbol)
import Data.Variant (Variant)
import Data.Variant as Variant
import Prim.Row as Row
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RL
import Type.Proxy (Proxy(..))

class OnlyVariant :: RowList Type -> Row Type -> Type -> Constraint
class OnlyVariant rl row a | rl -> row a where
  onlyImpl :: Proxy rl -> Variant row -> a

instance
  ( IsSymbol label
  , Row.Cons label a () row
  ) =>
  OnlyVariant (RL.Cons label a RL.Nil) row a where
  onlyImpl _ variant = Variant.on (Proxy :: _ label) identity Variant.case_ variant

only :: forall row rl a. RowToList row rl => OnlyVariant rl row a => Variant row -> a
only = onlyImpl (Proxy :: _ rl)

-- Typeclass to determine return type: unwrapped value for single option, Variant for multiple
class VariantOrValue :: RowList Type -> Row Type -> Type -> Constraint
class VariantOrValue rl row result | rl row -> result where
  variantOrValue :: Proxy rl -> Variant row -> result

-- Single element: return the unwrapped value
instance
  ( IsSymbol label
  , Row.Cons label a () row
  ) =>
  VariantOrValue (RL.Cons label a RL.Nil) row a where
  variantOrValue _ variant = Variant.on (Proxy :: _ label) identity Variant.case_ variant

-- Multiple elements: return the Variant as-is
instance VariantOrValue (RL.Cons label a (RL.Cons label2 a2 tail)) row (Variant row) where
  variantOrValue _ variant = variant
