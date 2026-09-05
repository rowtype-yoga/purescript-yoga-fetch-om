module Yoga.Fetch.Om.MakeRequest
  ( class MakeRequest
  , httpMethod
  , makeRequest
  , class BodyEncoding
  , encodingContentType
  , encodeBody
  ) where

import Prelude

import Data.HTTP.Method (Method(..))
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import JS.Fetch as Fetch
import JS.Fetch.Headers (Headers)
import JS.Fetch.Headers as Headers
import JS.Fetch.Request as Request
import JS.Fetch.RequestBody as Body
import JS.Fetch.RequestCache as Cache
import JS.Fetch.RequestCredentials as Credentials
import JS.Fetch.RequestMode as Mode
import JS.Fetch.ReferrerPolicy as ReferrerPolicy
import JS.Fetch.Referrer as Referrer
import JS.Fetch.Integrity (Integrity(..))
import JS.Fetch.Duplex as Duplex
import JS.Fetch.Response (Response) as FetchResponse
import Promise.Aff as Promise
import Type.Proxy (Proxy)
import Yoga.HTTP.API.Route.Encoding (JSON, FormData, PlainText, NoBody)
import Yoga.HTTP.API.Route.Method as Method
import Yoga.JSON (class WriteForeign, writeJSON)

class MakeRequest :: Type -> Constraint
class MakeRequest method where
  httpMethod :: Proxy method -> Method

instance MakeRequest Method.GET where
  httpMethod _ = GET

instance MakeRequest Method.POST where
  httpMethod _ = POST

instance MakeRequest Method.PUT where
  httpMethod _ = PUT

instance MakeRequest Method.DELETE where
  httpMethod _ = DELETE

instance MakeRequest Method.PATCH where
  httpMethod _ = PATCH

instance MakeRequest Method.QUERY where
  httpMethod _ = QUERY

makeRequest
  :: forall method
   . MakeRequest method
  => Proxy method
  -> String
  -> Headers
  -> String
  -> Maybe String
  -> Aff FetchResponse.Response
makeRequest proxy url customHeaders bodyContentType maybeBody = do
  request <- Request.new url options # liftEffect
  Promise.toAffE $ Fetch.fetch request
  where
  method = httpMethod proxy
  customArr = Headers.toArray customHeaders
  hasContentType = Array.any (\t -> fst t == "content-type") customArr
  contentTypeArr = case maybeBody of
    Nothing -> []
    Just _ | hasContentType -> []
    Just _ -> [ "Content-Type" /\ bodyContentType ]
  allHeaders = Headers.fromFoldable (contentTypeArr <> customArr)
  body = case maybeBody of
    Nothing -> Body.empty
    Just b -> Body.fromString b
  options =
    { method
    , headers: allHeaders
    , body
    , credentials: Credentials.SameOrigin
    , mode: Mode.Cors
    , referrer: Referrer.ReferrerUrl ""
    , referrerPolicy: ReferrerPolicy.NoReferrer
    , integrity: Integrity ""
    , duplex: Duplex.Half
    , cache: Cache.Default
    }

class BodyEncoding :: Type -> Type -> Constraint
class BodyEncoding encoding body | encoding -> body where
  encodingContentType :: Proxy encoding -> String
  encodeBody :: body -> Maybe String

instance WriteForeign ty => BodyEncoding (JSON ty) ty where
  encodingContentType _ = "application/json"
  encodeBody b = Just (writeJSON b)

else instance BodyEncoding PlainText String where
  encodingContentType _ = "text/plain"
  encodeBody = Just

else instance BodyEncoding (FormData ty) ty where
  encodingContentType _ = "application/x-www-form-urlencoded"
  encodeBody b = Just (toUrlEncoded b)

else instance BodyEncoding NoBody Unit where
  encodingContentType _ = ""
  encodeBody _ = Nothing

foreign import toUrlEncoded :: forall a. a -> String
