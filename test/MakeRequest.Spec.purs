module MakeRequest.Spec where

import Prelude

import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff)
import JS.Fetch.Headers as Headers
import Prim.RowList as RL
import Type.Proxy (Proxy(..))
import Yoga.Fetch.Om (toHeaders)
import ViTest (ViTest, describe, test)
import ViTest.Expect (expectToBe)

expectToEqual :: forall a. Eq a => a -> a -> Aff Unit
expectToEqual expected actual = expectToBe true (expected == actual)

testToHeaders :: Effect ViTest
testToHeaders = describe "toHeaders" $ do
  _ <- test "empty headers produces no entries" do
    let hdrs = toHeaders (Proxy :: _ RL.Nil) {}
    expectToEqual [] (Headers.toArray hdrs)

  _ <- test "single header is included" do
    let hdrs = toHeaders (Proxy :: _ (RL.Cons "authorization" String RL.Nil)) { authorization: "Bearer abc123" }
    expectToEqual [ "authorization" /\ "Bearer abc123" ] (Headers.toArray hdrs)

  test "multiple headers are included" do
    let
      hdrs = toHeaders
        (Proxy :: _ (RL.Cons "authorization" String (RL.Cons "x-custom" String RL.Nil)))
        { authorization: "Bearer abc123", "x-custom": "value" }
      arr = Headers.toArray hdrs
    expectToEqual true
      ( arr == [ "authorization" /\ "Bearer abc123", "x-custom" /\ "value" ]
          || arr == [ "x-custom" /\ "value", "authorization" /\ "Bearer abc123" ]
      )
