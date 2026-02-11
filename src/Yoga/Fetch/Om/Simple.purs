module Yoga.Fetch.Om.Simple
  ( class DecodeResponse
  , decodeResponse
  , FetchError
  , FetchResponse
  , get
  , getWithHeaders
  , delete
  , deleteWithHeaders
  , delete_
  , post
  , postWithHeaders
  , post_
  , put
  , putWithHeaders
  , put_
  , patch
  , patchWithHeaders
  , patch_
  ) where

import Prelude

import Data.HTTP.Method (Method(..))
import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\))
import Effect.Class (liftEffect)
import JS.Fetch as Fetch
import JS.Fetch.Duplex as Duplex
import JS.Fetch.Headers (Headers)
import JS.Fetch.Headers as Headers
import JS.Fetch.Integrity (Integrity(..))
import JS.Fetch.Referrer as Referrer
import JS.Fetch.ReferrerPolicy as ReferrerPolicy
import JS.Fetch.Request as Request
import JS.Fetch.RequestBody as Body
import JS.Fetch.RequestCache as Cache
import JS.Fetch.RequestCredentials as Credentials
import JS.Fetch.RequestMode as Mode
import JS.Fetch.Response as Resp
import Promise.Aff as Promise
import Type.Row.Homogeneous (class Homogeneous)
-- PlainText removed, using local phantom type
import Yoga.JSON (class ReadForeign, class WriteForeign, readJSON, writeJSON)
import Yoga.JSON.Error (withStringErrors)
import Yoga.Om (class ToOm, Om, toOm, throw)

type FetchError = { status :: Int, body :: String }

type FetchResponse a = { headers :: Headers, body :: a }

class DecodeResponse :: forall k. k -> Type -> Constraint
class DecodeResponse a result | a -> result where
  decodeResponse :: String -> Either String result

data PlainTextResponse

instance DecodeResponse PlainTextResponse String where
  decodeResponse = Right

else instance ReadForeign a => DecodeResponse a a where
  decodeResponse = withStringErrors <<< readJSON

simpleFetch
  :: forall @a result headers ctx err
   . DecodeResponse a result
  => Homogeneous headers String
  => Method
  -> Record headers
  -> String
  -> Maybe String
  -> Om ctx (fetchError :: FetchError | err) (FetchResponse result)
simpleFetch method headers url maybeBody = do
  resp <- doFetch # toOm
  respText <- Promise.toAffE (Resp.text resp) # toOm
  let status = Resp.status resp
  let respHeaders = Resp.headers resp
  if status >= 200 && status < 300 then
    case decodeResponse @a respText of
      Left errs -> throw { fetchError: { status, body: "Failed to parse response:\n" <> errs <> "\nBody: " <> respText } }
      Right body -> pure { headers: respHeaders, body }
  else
    throw { fetchError: { status, body: respText } }
  where
  doFetch = do
    request <- Request.new url options # liftEffect
    Promise.toAffE $ Fetch.fetch request
  customArr = Headers.toArray (Headers.fromRecord headers)
  hasContentType = Array.any (\t -> fst t == "content-type") customArr
  contentTypeArr = case maybeBody of
    Nothing -> []
    Just _ | hasContentType -> []
    Just _ -> [ "Content-Type" /\ "application/json" ]
  allHeaders = Headers.fromFoldable (contentTypeArr <> customArr)
  reqBody = case maybeBody of
    Nothing -> Body.empty
    Just b -> Body.fromString b
  options =
    { method
    , headers: allHeaders
    , body: reqBody
    , credentials: Credentials.SameOrigin
    , mode: Mode.Cors
    , referrer: Referrer.ReferrerNone
    , referrerPolicy: ReferrerPolicy.NoReferrer
    , integrity: Integrity ""
    , duplex: Duplex.Half
    , cache: Cache.Default
    }

get
  :: forall @a result headers ctx err
   . DecodeResponse a result
  => Homogeneous headers String
  => String
  -> Record headers
  -> Om ctx (fetchError :: FetchError | err) result
get url headers = _.body <$> simpleFetch @a GET headers url Nothing

getWithHeaders
  :: forall @a result headers ctx err
   . DecodeResponse a result
  => Homogeneous headers String
  => String
  -> Record headers
  -> Om ctx (fetchError :: FetchError | err) (FetchResponse result)
getWithHeaders url headers = simpleFetch @a GET headers url Nothing

delete
  :: forall @a result headers ctx err
   . DecodeResponse a result
  => Homogeneous headers String
  => String
  -> Record headers
  -> Om ctx (fetchError :: FetchError | err) result
delete url headers = _.body <$> simpleFetch @a DELETE headers url Nothing

deleteWithHeaders
  :: forall @a result headers ctx err
   . DecodeResponse a result
  => Homogeneous headers String
  => String
  -> Record headers
  -> Om ctx (fetchError :: FetchError | err) (FetchResponse result)
deleteWithHeaders url headers = simpleFetch @a DELETE headers url Nothing

delete_
  :: forall headers ctx err
   . Homogeneous headers String
  => String
  -> Record headers
  -> Om ctx (fetchError :: FetchError | err) Unit
delete_ url headers = void $ simpleFetch @PlainTextResponse DELETE headers url Nothing

post
  :: forall @a result headers body ctx err
   . DecodeResponse a result
  => Homogeneous headers String
  => WriteForeign body
  => String
  -> Record headers
  -> body
  -> Om ctx (fetchError :: FetchError | err) result
post url headers body = _.body <$> simpleFetch @a POST headers url (Just (writeJSON body))

postWithHeaders
  :: forall @a result headers body ctx err
   . DecodeResponse a result
  => Homogeneous headers String
  => WriteForeign body
  => String
  -> Record headers
  -> body
  -> Om ctx (fetchError :: FetchError | err) (FetchResponse result)
postWithHeaders url headers body = simpleFetch @a POST headers url (Just (writeJSON body))

post_
  :: forall headers body ctx err
   . Homogeneous headers String
  => WriteForeign body
  => String
  -> Record headers
  -> body
  -> Om ctx (fetchError :: FetchError | err) Unit
post_ url headers body = void $ simpleFetch @PlainTextResponse POST headers url (Just (writeJSON body))

put
  :: forall @a result headers body ctx err
   . DecodeResponse a result
  => Homogeneous headers String
  => WriteForeign body
  => String
  -> Record headers
  -> body
  -> Om ctx (fetchError :: FetchError | err) result
put url headers body = _.body <$> simpleFetch @a PUT headers url (Just (writeJSON body))

putWithHeaders
  :: forall @a result headers body ctx err
   . DecodeResponse a result
  => Homogeneous headers String
  => WriteForeign body
  => String
  -> Record headers
  -> body
  -> Om ctx (fetchError :: FetchError | err) (FetchResponse result)
putWithHeaders url headers body = simpleFetch @a PUT headers url (Just (writeJSON body))

put_
  :: forall headers body ctx err
   . Homogeneous headers String
  => WriteForeign body
  => String
  -> Record headers
  -> body
  -> Om ctx (fetchError :: FetchError | err) Unit
put_ url headers body = void $ simpleFetch @PlainTextResponse PUT headers url (Just (writeJSON body))

patch
  :: forall @a result headers body ctx err
   . DecodeResponse a result
  => Homogeneous headers String
  => WriteForeign body
  => String
  -> Record headers
  -> body
  -> Om ctx (fetchError :: FetchError | err) result
patch url headers body = _.body <$> simpleFetch @a PATCH headers url (Just (writeJSON body))

patchWithHeaders
  :: forall @a result headers body ctx err
   . DecodeResponse a result
  => Homogeneous headers String
  => WriteForeign body
  => String
  -> Record headers
  -> body
  -> Om ctx (fetchError :: FetchError | err) (FetchResponse result)
patchWithHeaders url headers body = simpleFetch @a PATCH headers url (Just (writeJSON body))

patch_
  :: forall headers body ctx err
   . Homogeneous headers String
  => WriteForeign body
  => String
  -> Record headers
  -> body
  -> Om ctx (fetchError :: FetchError | err) Unit
patch_ url headers body = void $ simpleFetch @PlainTextResponse PATCH headers url (Just (writeJSON body))
