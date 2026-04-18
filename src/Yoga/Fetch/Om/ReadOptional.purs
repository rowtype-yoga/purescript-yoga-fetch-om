module Yoga.Fetch.Om.ReadOptional
  ( readOptional
  , class ReadPartialQuery
  , readPartialQuery
  ) where

import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Prim.Row as Row
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RL
import Record as Record
import Type.Proxy (Proxy(..))

foreign import readOptionalImpl :: forall a r. String -> Record r -> Maybe a

readOptional :: forall @name ty r. IsSymbol name => Record r -> Maybe ty
readOptional rec = readOptionalImpl (reflectSymbol (Proxy :: _ name)) rec

-- | Read query params from a potentially-partial record.
-- | Walks the query params RowList:
-- |   Maybe ty → read with undefined-awareness (present → Just, absent → Nothing)
-- |   bare ty → read directly (required, always present)
class ReadPartialQuery (rl :: RowList Type) (queryParams :: Row Type) | rl -> queryParams where
  readPartialQuery :: forall r. Record r -> Record queryParams

instance ReadPartialQuery RL.Nil () where
  readPartialQuery _ = {}

-- Maybe ty: optional field, may be undefined at runtime
instance
  ( IsSymbol name
  , ReadPartialQuery tail tailQuery
  , Row.Cons name (Maybe ty) tailQuery queryParams
  , Row.Lacks name tailQuery
  ) =>
  ReadPartialQuery (RL.Cons name (Maybe ty) tail) queryParams where
  readPartialQuery rec = Record.insert (Proxy :: _ name) (readOptional @name rec) rest
    where
    rest = readPartialQuery @tail rec

-- bare ty: required field (from Required query params), always present
else instance
  ( IsSymbol name
  , ReadPartialQuery tail tailQuery
  , Row.Cons name ty tailQuery queryParams
  , Row.Lacks name tailQuery
  ) =>
  ReadPartialQuery (RL.Cons name ty tail) queryParams where
  readPartialQuery rec = Record.insert (Proxy :: _ name) val rest
    where
    val = readRequiredField (reflectSymbol (Proxy :: _ name)) rec
    rest = readPartialQuery @tail rec

foreign import readRequiredField :: forall a r. String -> Record r -> a
