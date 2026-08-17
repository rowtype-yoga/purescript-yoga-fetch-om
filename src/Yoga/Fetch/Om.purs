module Yoga.Fetch.Om
  ( client
  , clientWith
  , ClientInputs
  , ClientWithConfig
  , deriveClient
  , class DeriveClient
  , deriveClientImpl
  , class DeriveClientRL
  , deriveClientRL
  , class DeriveClientFn
  , deriveClientFn
  , class DeriveClientFnWith
  , deriveClientFnWith
  , class DeriveClientWith
  , deriveClientWithImpl
  , class DeriveClientWithRL
  , deriveClientWithRL
  , class RecordRow
  , class ToHeaders
  , toHeaders
  , module Yoga.HTTP.API.Route
  , module Yoga.HTTP.API.Path
  , plainText
  , module Yoga.Fetch.Om.StreamDecode
  , module Yoga.Fetch.Om.Simple
  ) where

import Prelude

import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple.Nested ((/\))
import Data.Variant (Variant)
import Prim.Row as Row
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RL
import Record as Record
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)
import JS.Fetch.Headers (Headers)
import JS.Fetch.Headers as Headers
import Yoga.Fetch.Om.BuildUrl (class BuildUrl, buildUrl)
import Yoga.Fetch.Om.Simple (class DecodeResponse, decodeResponse, FetchError, FetchResponse, get, getWithHeaders, delete, deleteWithHeaders, delete_, post, postWithHeaders, post_, put, putWithHeaders, put_, patch, patchWithHeaders, patch_)
import Yoga.Fetch.Om.StreamDecode (class StreamDecode, decodeStream)
import Yoga.Fetch.Om.ClientFunction (class BuildClientFn, class CheckBodyIsUnit, buildClientFn)
import Yoga.Fetch.Om.ExtractParams (class ExtractRequestBody, class ExtractBodyEncoding, class ExtractRequestHeaders)
import Yoga.Fetch.Om.MakeRequest (class MakeRequest, class BodyEncoding, makeRequest, encodingContentType, encodeBody)
import Yoga.Fetch.Om.ParseResponse (class ParseResponse, parseResponse)
import Yoga.Fetch.Om.SplitResponses (class SplitResponses)
import Yoga.Fetch.Om.Variant (class VariantOrValue, variantOrValue)
import Yoga.HTTP.API.Path (Path, Root, Lit, Capture, PathCons, Param, QueryParams, Required, type (/), type (:), type (:?), class PathPattern)
import Yoga.HTTP.API.Route (Route(..), GET, POST, PUT, DELETE, PATCH, Response(..), JSON, FormData, PlainText, Streaming, NoBody, class HeaderValue, printHeader)
import Yoga.HTTP.API.Route.Handler (class SegmentPathParams, class SegmentQueryParams)
import Yoga.Om (Om, toOm)

type ClientInputs providedHeaders providedQuery =
  { headers :: Record providedHeaders
  , query :: Record providedQuery
  }

type ClientWithConfig ctx err providedHeaders providedQuery =
  { baseUrl :: Om { | ctx } err String
  , provide :: Om { | ctx } err (ClientInputs providedHeaders providedQuery)
  }


foreign import mergeRecords :: forall left right out. Record left -> Record right -> Record out

-- | Extract row type from Record type
class RecordRow :: Type -> Row Type -> Constraint
class RecordRow t r | t -> r

instance RecordRow (Record r) r

class ToHeaders :: RowList Type -> Row Type -> Constraint
class ToHeaders headersRL headers where
  toHeaders :: Proxy headersRL -> Record headers -> Headers

instance ToHeaders RL.Nil headers where
  toHeaders _ _ = Headers.empty

else instance
  ( IsSymbol name
  , HeaderValue ty
  , Row.Cons name ty tailRow headers
  , Row.Lacks name tailRow
  , ToHeaders tail tailRow
  ) =>
  ToHeaders (RL.Cons name ty tail) headers where
  toHeaders _ headers = Headers.fromFoldable ([ headerName /\ headerValue ] <> Headers.toArray rest)
    where
    headerName = reflectSymbol (Proxy :: Proxy name)
    headerValue = printHeader (Record.get (Proxy :: Proxy name) headers)
    rest = toHeaders (Proxy :: Proxy tail) (unsafeCoerce headers :: Record tailRow)

class DeriveClientFn
  :: forall k1. Type -> k1 -> Type -> Row Type -> Row Type -> Row Type -> Row Type -> Type -> Type -> Constraint
class
  DeriveClientFn method segments request response ctx extraErr routeErrors result fn
  | method segments request response -> routeErrors result
  , method segments request response ctx extraErr -> fn where
  deriveClientFn :: String -> Proxy (Route method segments request response) -> fn

instance
  ( SegmentPathParams segments pathParams
  , SegmentQueryParams segments queryParams
  , ExtractRequestBody request body
  , ExtractBodyEncoding request encoding
  , ExtractRequestHeaders request headers
  , PathPattern segments
  , BuildUrl segments pathParams queryParams
  , MakeRequest method
  , BodyEncoding encoding body
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
  , Row.Union routeErrors extraErr errRow
  , BuildClientFn pathQueryRL headersRL bodyFlag body pathQuery headers (Record pathQuery) { | ctx } errRow result fn
  ) =>
  DeriveClientFn method segments request response ctx extraErr routeErrors result fn where
  deriveClientFn baseUrl _ =
    buildClientFn (Proxy :: _ pathQueryRL) (Proxy :: _ headersRL) (Proxy :: _ bodyFlag) identity impl
    where
    impl :: Record pathQuery -> Record headers -> body -> Om { | ctx } errRow result
    impl pathQueryRec headersRec bodyVal = widenOm do
      let url = buildUrl baseUrl (Proxy :: _ segments) pathParamsRec queryParamsRec
      let hdrs = toHeaders (Proxy :: _ headersRL) headersRec
      let ct = encodingContentType (Proxy :: _ encoding)
      fetchResp <- makeRequest (Proxy :: _ method) url hdrs ct (encodeBody @encoding bodyVal) # toOm
      variant <- parseResponse fetchResp :: Om (Record ()) routeErrors (Variant successRow)
      variantOrValue (Proxy :: _ successRL) variant # pure
      where
      pathParamsRec = unsafeCoerce pathQueryRec :: Record pathParams
      queryParamsRec = unsafeCoerce pathQueryRec :: Record queryParams
      widenOm :: Om (Record ()) routeErrors ~> Om { | ctx } errRow
      widenOm = unsafeCoerce

class DeriveClientFnWith
  :: forall k1. Type -> k1 -> Type -> Row Type -> Row Type -> Row Type -> Row Type -> Row Type -> Row Type -> Type -> Type -> Constraint
class
  DeriveClientFnWith method segments request response providedHeaders providedQuery ctx extraErr routeErrors result fn
  | method segments request response -> routeErrors result
  , method segments request response providedHeaders providedQuery ctx extraErr -> fn where
  deriveClientFnWith
    :: ClientWithConfig ctx extraErr providedHeaders providedQuery
    -> Proxy (Route method segments request response)
    -> fn

instance
  ( SegmentPathParams segments pathParams
  , SegmentQueryParams segments queryParams
  , ExtractRequestBody request body
  , ExtractBodyEncoding request encoding
  , ExtractRequestHeaders request headers
  , PathPattern segments
  , BuildUrl segments pathParams queryParams
  , MakeRequest method
  , BodyEncoding encoding body
  , SplitResponses response successRow routeErrors
  , Row.Union pathParams queryParams pathQuery
  , Row.Nub pathQuery pathQuery
  , ParseResponse routeErrors successRow
  , RowToList successRow successRL
  , VariantOrValue successRL successRow result
  , Row.Union routeErrors extraErr errRow
  , Row.Union providedQuery remainingQuery queryParams
  , Row.Union pathParams remainingQuery callerPathQuery
  , Row.Nub callerPathQuery callerPathQuery
  , RowToList callerPathQuery callerPathQueryRL
  , RowToList headers headersRL
  , ToHeaders headersRL headers
  , Row.Union providedHeaders remainingHeaders headers
  , Row.Nub headers headers
  , RowToList remainingHeaders remainingHeadersRL
  , CheckBodyIsUnit body bodyFlag
  , BuildClientFn callerPathQueryRL remainingHeadersRL bodyFlag body callerPathQuery remainingHeaders (Record callerPathQuery) { | ctx } errRow result fn
  ) =>
  DeriveClientFnWith method segments request response providedHeaders providedQuery ctx extraErr routeErrors result fn where
  deriveClientFnWith config _ =
    buildClientFn (Proxy :: _ callerPathQueryRL) (Proxy :: _ remainingHeadersRL) (Proxy :: _ bodyFlag) identity impl
    where
    impl :: Record callerPathQuery -> Record remainingHeaders -> body -> Om { | ctx } errRow result
    impl pathQueryRec headersRec bodyVal = do
      baseUrl <- widenExtra config.baseUrl
      provided <- widenExtra config.provide
      let pathParamsRec = unsafeCoerce pathQueryRec :: Record pathParams
      let queryParamsRec = mergeRecords provided.query (unsafeCoerce pathQueryRec :: Record remainingQuery) :: Record queryParams
      let fullHeaders = mergeRecords provided.headers headersRec :: Record headers
      let url = buildUrl baseUrl (Proxy :: _ segments) pathParamsRec queryParamsRec
      let hdrs = toHeaders (Proxy :: _ headersRL) fullHeaders
      let ct = encodingContentType (Proxy :: _ encoding)
      fetchResp <- makeRequest (Proxy :: _ method) url hdrs ct (encodeBody @encoding bodyVal) # toOm # widenRoute
      variant <- parseResponse fetchResp # widenRoute :: Om { | ctx } errRow (Variant successRow)
      variantOrValue (Proxy :: _ successRL) variant # pure
      where
      widenExtra :: Om { | ctx } extraErr ~> Om { | ctx } errRow
      widenExtra = unsafeCoerce
      widenRoute :: Om (Record ()) routeErrors ~> Om { | ctx } errRow
      widenRoute = unsafeCoerce

class DeriveClient :: Row Type -> Row Type -> Row Type -> Row Type -> Constraint
class DeriveClient ctx extraErr routesRow clientsRow | routesRow ctx extraErr -> clientsRow where
  deriveClientImpl :: String -> Proxy (Record routesRow) -> Record clientsRow

-- | Derive API client functions from route definitions using VTA.
-- | Generated fields use ordinary function application. Path parameters are
-- | plain records; optional query parameters are explicit `Maybe` fields.
-- |
-- | ```purescript
-- | type UserAPI = { getUser :: Route ... }
-- | api = client @UserAPI "https://api.example.com"
-- | user <- api.getUser { id: 42 }
-- | users <- api.listUsers { limit: Just 10, offset: Nothing }
-- | health <- api.health
-- | ```
client :: forall @routes routesRow ctx extraErr clientsRow. RecordRow routes routesRow => DeriveClient ctx extraErr routesRow clientsRow => String -> Record clientsRow
client baseUrl = deriveClientImpl @ctx @extraErr baseUrl (Proxy :: _ { | routesRow })

-- | Deprecated: Use `client` with VTA instead
-- |
-- | ```purescript
-- | api = deriveClient @UserAPI "https://api.example.com"
-- | ```
deriveClient :: forall @routesRow ctx extraErr clientsRow. DeriveClient ctx extraErr routesRow clientsRow => String -> Record clientsRow
deriveClient baseUrl = deriveClientImpl @ctx @extraErr baseUrl (Proxy :: _ { | routesRow })

clientWith
  :: forall @routes @providedHeaders @providedQuery routesRow ctx extraErr clientsRow
   . RecordRow routes routesRow
  => DeriveClientWith providedHeaders providedQuery ctx extraErr routesRow clientsRow
  => ClientWithConfig ctx extraErr providedHeaders providedQuery
  -> Record clientsRow
clientWith config = deriveClientWithImpl @providedHeaders @providedQuery @ctx @extraErr config (Proxy :: _ { | routesRow })


plainText :: PlainText -> String
plainText = unsafeCoerce

instance
  ( RowToList routesRow rl
  , DeriveClientRL ctx extraErr rl () clientsRow
  ) =>
  DeriveClient ctx extraErr routesRow clientsRow where
  deriveClientImpl baseUrl _ = deriveClientRL @ctx @extraErr baseUrl (Proxy :: _ rl) {}

class DeriveClientWith :: Row Type -> Row Type -> Row Type -> Row Type -> Row Type -> Row Type -> Constraint
class DeriveClientWith providedHeaders providedQuery ctx extraErr routesRow clientsRow | providedHeaders providedQuery routesRow ctx extraErr -> clientsRow where
  deriveClientWithImpl :: ClientWithConfig ctx extraErr providedHeaders providedQuery -> Proxy (Record routesRow) -> Record clientsRow

instance
  ( RowToList routesRow rl
  , DeriveClientWithRL providedHeaders providedQuery ctx extraErr rl () clientsRow
  ) =>
  DeriveClientWith providedHeaders providedQuery ctx extraErr routesRow clientsRow where
  deriveClientWithImpl config _ = deriveClientWithRL @providedHeaders @providedQuery @ctx @extraErr config (Proxy :: _ rl) {}

class DeriveClientWithRL :: Row Type -> Row Type -> Row Type -> Row Type -> RowList Type -> Row Type -> Row Type -> Constraint
class DeriveClientWithRL providedHeaders providedQuery ctx extraErr rl acc out | providedHeaders providedQuery ctx extraErr rl acc -> out where
  deriveClientWithRL :: ClientWithConfig ctx extraErr providedHeaders providedQuery -> Proxy rl -> Record acc -> Record out

instance DeriveClientWithRL providedHeaders providedQuery ctx extraErr RL.Nil acc acc where
  deriveClientWithRL _ _ acc = acc

instance
  ( IsSymbol label
  , DeriveClientFnWith method segments request response providedHeaders providedQuery ctx extraErr routeErrors result fn
  , DeriveClientWithRL providedHeaders providedQuery ctx extraErr tail acc1 acc2
  , Row.Cons label fn acc2 out
  , Row.Lacks label acc2
  , Row.Cons label (Route method segments request response) routeTail routeRow
  ) =>
  DeriveClientWithRL providedHeaders providedQuery ctx extraErr (RL.Cons label (Route method segments request response) tail) acc1 out where
  deriveClientWithRL config _ acc = Record.insert (Proxy :: _ label) clientFn rest
    where
    clientFn = deriveClientFnWith @_ @_ @_ @_ @providedHeaders @providedQuery @ctx @extraErr config (Proxy :: _ (Route method segments request response))
    rest = deriveClientWithRL @providedHeaders @providedQuery @ctx @extraErr config (Proxy :: _ tail) acc

class DeriveClientRL :: Row Type -> Row Type -> RowList Type -> Row Type -> Row Type -> Constraint
class DeriveClientRL ctx extraErr rl acc out | ctx extraErr rl acc -> out where
  deriveClientRL :: String -> Proxy rl -> Record acc -> Record out

instance DeriveClientRL ctx extraErr RL.Nil acc acc where
  deriveClientRL _ _ acc = acc

instance
  ( IsSymbol label
  , DeriveClientFn method segments request response ctx extraErr routeErrors result fn
  , DeriveClientRL ctx extraErr tail acc1 acc2
  , Row.Cons label fn acc2 out
  , Row.Lacks label acc2
  , Row.Cons label (Route method segments request response) routeTail routeRow
  ) =>
  DeriveClientRL ctx extraErr (RL.Cons label (Route method segments request response) tail) acc1 out where
  deriveClientRL baseUrl _ acc = Record.insert (Proxy :: _ label) clientFn rest
    where
    clientFn = deriveClientFn @_ @_ @_ @_ @ctx @extraErr baseUrl (Proxy :: _ (Route method segments request response))
    rest = deriveClientRL @ctx @extraErr baseUrl (Proxy :: _ tail) acc
