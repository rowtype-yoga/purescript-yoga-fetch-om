module Yoga.Fetch.Om
  ( client
  , deriveClient
  , class DeriveClient
  , deriveClientImpl
  , class DeriveClientRL
  , deriveClientRL
  , class DeriveClientFn
  , deriveClientFn
  , class RecordRow
  , class ToHeaders
  , toHeaders
  , module Yoga.HTTP.API.Route
  , module Yoga.HTTP.API.Path
  , module Yoga.Fetch.Om.Simple
  ) where

import Prelude

import Data.Symbol (class IsSymbol)
import Data.Variant (Variant)
import Prim.Row as Row
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RL
import Record as Record
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)
import JS.Fetch.Headers (Headers)
import JS.Fetch.Headers as Headers
import Type.Row.Homogeneous (class Homogeneous)
import Yoga.Fetch.Om.BuildUrl (class BuildUrl, buildUrl)
import Yoga.Fetch.Om.Simple (class DecodeResponse, decodeResponse, FetchError, FetchResponse, get, getWithHeaders, delete, deleteWithHeaders, delete_, post, postWithHeaders, post_, put, putWithHeaders, put_, patch, patchWithHeaders, patch_)
import Yoga.Fetch.Om.ClientFn (class BuildClientFn, class CheckBodyIsUnit, buildClientFn)
import Yoga.Fetch.Om.ExtractParams (class ExtractRequestBody, class ExtractRequestHeaders)
import Yoga.Fetch.Om.MakeRequest (class MakeRequest, class SerializeBody, makeRequest, serializeBody)
import Yoga.Fetch.Om.ParseResponse (class ParseResponse, parseResponse)
import Yoga.Fetch.Om.SplitResponses (class SplitResponses)
import Yoga.Fetch.Om.Variant (class VariantOrValue, variantOrValue)
import Yoga.HTTP.API.Path (Path, Root, Lit, Capture, PathCons, Param, QueryParams, Required, type (/), type (:), type (:?), class PathPattern)
import Yoga.HTTP.API.Route (Route(..), GET, POST, PUT, DELETE, PATCH, Response(..), JSON, FormData, NoBody)
import Yoga.HTTP.API.Route.Handler (class SegmentPathParams, class SegmentQueryParams)
import Yoga.Om (Om, fromAff)

-- | Extract row type from Record type
class RecordRow :: Type -> Row Type -> Constraint
class RecordRow t r | t -> r

instance RecordRow (Record r) r

class ToHeaders :: RowList Type -> Row Type -> Constraint
class ToHeaders headersRL headers where
  toHeaders :: Proxy headersRL -> Record headers -> Headers

instance ToHeaders RL.Nil headers where
  toHeaders _ _ = Headers.empty

else instance Homogeneous headers String => ToHeaders headersRL headers where
  toHeaders _ = Headers.fromRecord

class DeriveClientFn :: forall k1. Type -> k1 -> Type -> Row Type -> Row Type -> Row Type -> Type -> Type -> Constraint
class
  DeriveClientFn method segments request response routeErrors successRow result fn
  | method segments request response -> routeErrors successRow result fn where
  deriveClientFn :: String -> Proxy (Route method segments request response) -> fn

instance
  ( SegmentPathParams segments pathParams
  , SegmentQueryParams segments queryParams
  , ExtractRequestBody request body
  , ExtractRequestHeaders request headers
  , PathPattern segments
  , BuildUrl segments pathParams queryParams
  , MakeRequest method
  , SerializeBody body
  , SplitResponses response successRow routeErrors
  , ParseResponse routeErrors successRow
  , RowToList successRow successRL
  , VariantOrValue successRL successRow result
  , Row.Union pathParams queryParams pathQuery
  , Row.Nub pathQuery pathQuery
  , RowToList pathQuery pathQueryRL
  , RowToList headers headersRL
  , ToHeaders headersRL headers
  , CheckBodyIsUnit body bodyFlag
  , BuildClientFn pathQueryRL headersRL bodyFlag body pathQuery headers routeErrors result fn
  ) =>
  DeriveClientFn method segments request response routeErrors successRow result fn where
  deriveClientFn baseUrl _ =
    buildClientFn (Proxy :: _ pathQueryRL) (Proxy :: _ headersRL) (Proxy :: _ bodyFlag) impl
    where
    impl :: Record pathQuery -> Record headers -> body -> Om (Record ()) routeErrors result
    impl pathQueryRec headersRec bodyVal = do
      let url = buildUrl baseUrl (Proxy :: _ segments) pathParamsRec queryParamsRec
      let hdrs = toHeaders (Proxy :: _ headersRL) headersRec
      fetchResp <- makeRequest (Proxy :: _ method) url hdrs (serializeBody bodyVal) # fromAff
      variant <- parseResponse fetchResp :: Om (Record ()) routeErrors (Variant successRow)
      variantOrValue (Proxy :: _ successRL) variant # pure
      where
      pathParamsRec = unsafeCoerce pathQueryRec :: Record pathParams
      queryParamsRec = unsafeCoerce pathQueryRec :: Record queryParams

class DeriveClient :: Row Type -> Row Type -> Constraint
class DeriveClient routesRow clientsRow | routesRow -> clientsRow where
  deriveClientImpl :: String -> Proxy (Record routesRow) -> Record clientsRow

-- | Derive API client functions from route definitions using VTA
-- |
-- | ```purescript
-- | type UserAPI = { getUser :: Route ... }
-- | api = client @UserAPI "https://api.example.com"
-- | ```
client :: forall @routes routesRow clientsRow. RecordRow routes routesRow => DeriveClient routesRow clientsRow => String -> Record clientsRow
client baseUrl = deriveClientImpl baseUrl (Proxy :: _ { | routesRow })

-- | Deprecated: Use `client` with VTA instead
-- |
-- | ```purescript
-- | api = deriveClient @UserAPI "https://api.example.com"
-- | ```
deriveClient :: forall @routesRow clientsRow. DeriveClient routesRow clientsRow => String -> Record clientsRow
deriveClient baseUrl = deriveClientImpl baseUrl (Proxy :: _ { | routesRow })

instance
  ( RowToList routesRow rl
  , DeriveClientRL rl () clientsRow
  ) =>
  DeriveClient routesRow clientsRow where
  deriveClientImpl baseUrl _ = deriveClientRL baseUrl (Proxy :: _ rl) {}

class DeriveClientRL :: RowList Type -> Row Type -> Row Type -> Constraint
class DeriveClientRL rl acc out | rl acc -> out where
  deriveClientRL :: String -> Proxy rl -> Record acc -> Record out

instance DeriveClientRL RL.Nil acc acc where
  deriveClientRL _ _ acc = acc

instance
  ( IsSymbol label
  , DeriveClientFn method segments request response routeErrors successRow result fn
  , DeriveClientRL tail acc1 acc2
  , Row.Cons label fn acc2 out
  , Row.Lacks label acc2
  , Row.Cons label (Route method segments request response) routeTail routeRow
  ) =>
  DeriveClientRL (RL.Cons label (Route method segments request response) tail) acc1 out where
  deriveClientRL baseUrl _ acc = Record.insert (Proxy :: _ label) clientFn rest
    where
    clientFn = deriveClientFn baseUrl (Proxy :: _ (Route method segments request response))
    rest = deriveClientRL baseUrl (Proxy :: _ tail) acc
