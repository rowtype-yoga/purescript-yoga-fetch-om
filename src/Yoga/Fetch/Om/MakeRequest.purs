module Yoga.Fetch.Om.MakeRequest
  ( class MakeRequest
  , httpMethod
  , makeRequest
  , class SerializeBody
  , serializeBody
  ) where

import Prelude

import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import JS.Fetch as Fetch
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

makeRequest
  :: forall method
   . MakeRequest method
  => Proxy method
  -> String
  -> Maybe String
  -> Aff FetchResponse.Response
makeRequest proxy url maybeBody = do
  request <- Request.new url options # liftEffect
  Promise.toAffE $ Fetch.fetch request
  where
  method = httpMethod proxy
  headers = case maybeBody of
    Nothing -> Headers.empty
    Just _ -> Headers.fromFoldable [ "Content-Type" /\ "application/json" ]
  body = case maybeBody of
    Nothing -> Body.empty
    Just b -> Body.fromString b
  options =
    { method
    , headers
    , body
    , credentials: Credentials.Include
    , mode: Mode.Cors
    , referrer: Referrer.ReferrerClient
    , referrerPolicy: ReferrerPolicy.NoReferrer
    , integrity: Integrity ""
    , duplex: Duplex.Half
    , cache: Cache.Default
    }

class SerializeBody :: Type -> Constraint
class SerializeBody body where
  serializeBody :: body -> Maybe String

instance SerializeBody Unit where
  serializeBody _ = Nothing
else instance WriteForeign body => SerializeBody body where
  serializeBody b = Just (writeJSON b)
