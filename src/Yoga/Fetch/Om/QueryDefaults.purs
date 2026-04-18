module Yoga.Fetch.Om.QueryDefaults
  ( class BuildMaybeQuery
  , buildMaybeQuery
  , class HasField
  , class BuildMaybeQueryField
  , buildMaybeQueryField
  , class SplitQueryRow
  , class SplitQueryRowRL
  , class BuildQueryFromPartial
  , buildQueryFromPartial
  , class BuildQueryFromPartialRL
  , buildQueryFromPartialRL
  ) where

import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol)
import Prim.Boolean (True, False)
import Prim.Row as Row
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RL
import Record as Record
import Type.Proxy (Proxy(..))

-- | Check whether a label exists in a RowList
class HasField (name :: Symbol) (rl :: RowList Type) (has :: Boolean) | name rl -> has

instance HasField name RL.Nil False
instance HasField name (RL.Cons name ty tail) True
else instance HasField name tail has => HasField name (RL.Cons other ty tail) has

-- | Get a Maybe-wrapped value from a record, based on whether the field exists
class BuildMaybeQueryField (has :: Boolean) (name :: Symbol) (ty :: Type) (provided :: Row Type) where
  buildMaybeQueryField :: Proxy has -> Proxy name -> Record provided -> Maybe ty

instance (IsSymbol name, Row.Cons name ty rest provided) =>
  BuildMaybeQueryField True name ty provided where
  buildMaybeQueryField _ _ provided = Just (Record.get (Proxy :: _ name) provided)

instance BuildMaybeQueryField False name ty provided where
  buildMaybeQueryField _ _ _ = Nothing

-- | Build a full Maybe-wrapped query record from a partial provided record.
-- | Fields present in provided get Just, missing fields get Nothing.
class BuildMaybeQuery (rl :: RowList Type) (providedRL :: RowList Type) (provided :: Row Type) (queryParams :: Row Type) | rl -> queryParams where
  buildMaybeQuery :: Proxy rl -> Proxy providedRL -> Record provided -> Record queryParams

instance BuildMaybeQuery RL.Nil providedRL provided () where
  buildMaybeQuery _ _ _ = {}

instance
  ( IsSymbol name
  , HasField name providedRL has
  , BuildMaybeQueryField has name ty provided
  , BuildMaybeQuery tail providedRL provided tailQuery
  , Row.Cons name (Maybe ty) tailQuery queryParams
  , Row.Lacks name tailQuery
  ) =>
  BuildMaybeQuery (RL.Cons name ty tail) providedRL provided queryParams where
  buildMaybeQuery _ prl provided = Record.insert (Proxy :: _ name) val rest
    where
    val = buildMaybeQueryField (Proxy :: _ has) (Proxy :: _ name) provided
    rest = buildMaybeQuery (Proxy :: _ tail) prl provided

-- | Split a query params row into optional raw types and required raw types.
-- | Maybe ty → optional with raw type ty
-- | ty (no Maybe) → required with raw type ty (from Required query params)
class SplitQueryRow (queryParams :: Row Type) (optionalRaw :: Row Type) (requiredRaw :: Row Type) | queryParams -> optionalRaw requiredRaw

instance (RowToList queryParams rl, SplitQueryRowRL rl optionalRaw requiredRaw) => SplitQueryRow queryParams optionalRaw requiredRaw

class SplitQueryRowRL (rl :: RowList Type) (optionalRaw :: Row Type) (requiredRaw :: Row Type) | rl -> optionalRaw requiredRaw

instance SplitQueryRowRL RL.Nil () ()

-- Maybe ty field → optional
instance
  ( SplitQueryRowRL tail optTail reqTail
  , Row.Cons name ty optTail optionalRaw
  , Row.Lacks name optTail
  ) =>
  SplitQueryRowRL (RL.Cons name (Maybe ty) tail) optionalRaw reqTail

-- Non-Maybe field → required (from Required query params)
else instance
  ( SplitQueryRowRL tail optTail reqTail
  , Row.Cons name ty reqTail requiredRaw
  , Row.Lacks name reqTail
  ) =>
  SplitQueryRowRL (RL.Cons name ty tail) optTail requiredRaw

-- | Build full queryParams from a partial record that contains path params,
-- | required query params, and optionally some optional query params.
-- | Walks the queryParams RowList:
-- |   Maybe ty field → check if raw value provided, wrap in Just or Nothing
-- |   bare ty field → copy from provided (required, always present)
class BuildQueryFromPartial (queryParams :: Row Type) (provided :: Row Type) where
  buildQueryFromPartial :: Record provided -> Record queryParams

instance (RowToList queryParams rl, RowToList provided prl, BuildQueryFromPartialRL rl prl provided queryParams) => BuildQueryFromPartial queryParams provided where
  buildQueryFromPartial = buildQueryFromPartialRL (Proxy :: _ rl) (Proxy :: _ prl)

class BuildQueryFromPartialRL (rl :: RowList Type) (providedRL :: RowList Type) (provided :: Row Type) (queryParams :: Row Type) | rl -> queryParams where
  buildQueryFromPartialRL :: Proxy rl -> Proxy providedRL -> Record provided -> Record queryParams

instance BuildQueryFromPartialRL RL.Nil providedRL provided () where
  buildQueryFromPartialRL _ _ _ = {}

-- Maybe ty: check if field is provided, wrap in Just or use Nothing
instance
  ( IsSymbol name
  , HasField name providedRL has
  , BuildMaybeQueryField has name ty provided
  , BuildQueryFromPartialRL tail providedRL provided tailQuery
  , Row.Cons name (Maybe ty) tailQuery queryParams
  , Row.Lacks name tailQuery
  ) =>
  BuildQueryFromPartialRL (RL.Cons name (Maybe ty) tail) providedRL provided queryParams where
  buildQueryFromPartialRL _ prl provided = Record.insert (Proxy :: _ name) val rest
    where
    val = buildMaybeQueryField (Proxy :: _ has) (Proxy :: _ name) provided
    rest = buildQueryFromPartialRL (Proxy :: _ tail) prl provided

-- bare ty (Required): always present in provided, copy directly
else instance
  ( IsSymbol name
  , Row.Cons name ty rest provided
  , BuildQueryFromPartialRL tail providedRL provided tailQuery
  , Row.Cons name ty tailQuery queryParams
  , Row.Lacks name tailQuery
  ) =>
  BuildQueryFromPartialRL (RL.Cons name ty tail) providedRL provided queryParams where
  buildQueryFromPartialRL _ prl provided = Record.insert (Proxy :: _ name) val rest
    where
    val = Record.get (Proxy :: _ name) provided
    rest = buildQueryFromPartialRL (Proxy :: _ tail) prl provided
