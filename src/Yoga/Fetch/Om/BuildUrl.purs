module Yoga.Fetch.Om.BuildUrl
  ( class BuildUrl
  , buildUrl
  , class SubstitutePathParams
  , substitutePathParams
  , substitutePathParamsImpl
  , class AppendQueryParams
  , appendQueryParams
  , appendQueryParamsImpl
  , class AppendQueryParamsRL
  , appendQueryParamsRL
  , class SubstitutePathParamsRL
  , substitutePathParamsRL
  , class SerializeParam
  , serializeParam
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), Replacement(..))
import Data.String as String
import Data.Symbol (class IsSymbol, reflectSymbol)
import Prim.Row as Row
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RL
import Record as Record
import Type.Proxy (Proxy(..))
import Yoga.HTTP.API.Path (class PathPattern, pathPattern)

class BuildUrl :: forall k. k -> Row Type -> Row Type -> Constraint
class BuildUrl segments pathParams queryParams | segments -> pathParams queryParams where
  buildUrl :: String -> Proxy segments -> Record pathParams -> Record queryParams -> String

instance
  ( PathPattern segments
  , SubstitutePathParams pathParams
  , AppendQueryParams queryParams
  ) =>
  BuildUrl segments pathParams queryParams where
  buildUrl baseUrl segmentsProxy pathParamsRec queryParamsRec = withQueryParams
    where
    pattern = pathPattern segmentsProxy
    withBase = case baseUrl of
      "" -> pattern
      base -> base <> pattern
    withPathParams = substitutePathParamsImpl (Proxy :: _ pathParams) pathParamsRec withBase
    withQueryParams = appendQueryParamsImpl (Proxy :: _ queryParams) queryParamsRec withPathParams

class SubstitutePathParams :: Row Type -> Constraint
class SubstitutePathParams params where
  substitutePathParamsImpl :: Proxy params -> Record params -> String -> String

substitutePathParams :: forall @params. SubstitutePathParams params => Record params -> String -> String
substitutePathParams = substitutePathParamsImpl (Proxy :: _ params)

instance
  ( RowToList params rl
  , SubstitutePathParamsRL rl params
  ) =>
  SubstitutePathParams params where
  substitutePathParamsImpl _ = substitutePathParamsRL (Proxy :: _ rl)

class SubstitutePathParamsRL :: RowList Type -> Row Type -> Constraint
class SubstitutePathParamsRL rl r where
  substitutePathParamsRL :: Proxy rl -> Record r -> String -> String

instance SubstitutePathParamsRL RL.Nil r where
  substitutePathParamsRL _ _ url = url

instance
  ( IsSymbol name
  , SerializeParam ty
  , SubstitutePathParamsRL tail r
  , Row.Cons name ty tailRow r
  ) =>
  SubstitutePathParamsRL (RL.Cons name ty tail) r where
  substitutePathParamsRL _ rec url = substitutePathParamsRL (Proxy :: _ tail) rec replaced
    where
    value = Record.get (Proxy :: _ name) rec
    pattern = Pattern (":" <> reflectSymbol (Proxy :: _ name))
    replaced = String.replaceAll pattern (Replacement (serializeParam value)) url

class AppendQueryParams :: Row Type -> Constraint
class AppendQueryParams params where
  appendQueryParamsImpl :: Proxy params -> Record params -> String -> String

appendQueryParams :: forall @params. AppendQueryParams params => Record params -> String -> String
appendQueryParams = appendQueryParamsImpl (Proxy :: _ params)

instance
  ( RowToList params rl
  , AppendQueryParamsRL rl params
  ) =>
  AppendQueryParams params where
  appendQueryParamsImpl _ rec url = url <> queryString
    where
    pairs = appendQueryParamsRL (Proxy :: _ rl) rec []
    queryString = case pairs of
      [] -> ""
      _ -> "?" <> String.joinWith "&" pairs

class AppendQueryParamsRL :: RowList Type -> Row Type -> Constraint
class AppendQueryParamsRL rl r where
  appendQueryParamsRL :: Proxy rl -> Record r -> Array String -> Array String

instance AppendQueryParamsRL RL.Nil r where
  appendQueryParamsRL _ _ acc = acc

instance
  ( IsSymbol name
  , SerializeParam ty
  , AppendQueryParamsRL tail r
  , Row.Cons name (Maybe ty) tailRow r
  ) =>
  AppendQueryParamsRL (RL.Cons name (Maybe ty) tail) r where
  appendQueryParamsRL _ rec acc = appendQueryParamsRL (Proxy :: _ tail) rec acc'
    where
    acc' = case Record.get (Proxy :: _ name) rec of
      Nothing -> acc
      Just value -> Array.snoc acc (reflectSymbol (Proxy :: _ name) <> "=" <> serializeParam value)

else instance
  ( IsSymbol name
  , SerializeParam ty
  , AppendQueryParamsRL tail r
  , Row.Cons name ty tailRow r
  ) =>
  AppendQueryParamsRL (RL.Cons name ty tail) r where
  appendQueryParamsRL _ rec acc =
    appendQueryParamsRL (Proxy :: _ tail) rec (Array.snoc acc pair)
    where
    pair = reflectSymbol (Proxy :: _ name) <> "=" <> serializeParam (Record.get (Proxy :: _ name) rec)

class SerializeParam :: Type -> Constraint
class SerializeParam a where
  serializeParam :: a -> String

instance SerializeParam String where
  serializeParam = identity

instance SerializeParam Int where
  serializeParam = show

instance SerializeParam Number where
  serializeParam = show

instance SerializeParam Boolean where
  serializeParam = show
