module MakeRequest.Spec where

import Prelude

import Data.Tuple.Nested ((/\))
import JS.Fetch.Headers as Headers
import Prim.RowList as RL
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import Type.Proxy (Proxy(..))
import Yoga.Fetch.Om (toHeaders)

spec :: Spec Unit
spec = describe "toHeaders" do
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
