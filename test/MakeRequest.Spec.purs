module MakeRequest.Spec where

import Prelude

import Data.HTTP.Method as HTTP
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import JS.Fetch.Headers as Headers
import Prim.RowList as RL
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import Type.Proxy (Proxy(..))
import Yoga.Fetch.Om (FormData, PlainText, QUERY, toHeaders)
import Yoga.Fetch.Om.MakeRequest (encodeBody, encodingContentType, httpMethod)
import Yoga.HTTP.API.Route (BearerToken(..))

spec :: Spec Unit
spec = do
  describe "MakeRequest" do
    it "maps a QUERY route method imported from the public module" do
      httpMethod (Proxy :: Proxy QUERY) `shouldEqual` HTTP.QUERY

  describe "BodyEncoding" do
    it "sends PlainText request bodies without JSON quoting" do
      encodingContentType (Proxy :: Proxy PlainText) `shouldEqual` "text/plain"
      encodeBody @PlainText "hello" `shouldEqual` Just "hello"

    it "URL-encodes FormData request bodies" do
      encodingContentType (Proxy :: Proxy (FormData { username :: String })) `shouldEqual` "application/x-www-form-urlencoded"
      encodeBody @(FormData { username :: String }) { username: "Ada Lovelace" }
        `shouldEqual` Just "username=Ada+Lovelace"

  describe "toHeaders" do
    it "empty headers produces no entries" do
      let hdrs = toHeaders (Proxy :: _ RL.Nil) {}
      Headers.toArray hdrs `shouldEqual` []

    it "single header is included" do
      let hdrs = toHeaders (Proxy :: _ (RL.Cons "authorization" String RL.Nil)) { authorization: "Bearer abc123" }
      Headers.toArray hdrs `shouldEqual` [ "authorization" /\ "Bearer abc123" ]

    it "multiple headers are included" do
      let
        hdrs = toHeaders
          (Proxy :: _ (RL.Cons "authorization" String (RL.Cons "x-custom" String RL.Nil)))
          { authorization: "Bearer abc123", "x-custom": "value" }
        arr = Headers.toArray hdrs
      arr `shouldSatisfy` \a ->
        a == [ "authorization" /\ "Bearer abc123", "x-custom" /\ "value" ]
          || a == [ "x-custom" /\ "value", "authorization" /\ "Bearer abc123" ]

    it "typed header values are rendered with HeaderValue" do
      let hdrs = toHeaders (Proxy :: _ (RL.Cons "authorization" BearerToken RL.Nil)) { authorization: BearerToken "abc123" }
      Headers.toArray hdrs `shouldEqual` [ "authorization" /\ "Bearer abc123" ]
