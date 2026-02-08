module Variant.Spec where

import Prelude

import Data.Variant (Variant)
import Data.Variant as Variant
import Effect (Effect)
import Effect.Aff (Aff)
import Type.Proxy (Proxy(..))
import ViTest (ViTest, describe, test)
import ViTest.Expect (expectToBe)
import Yoga.Fetch.Om.Variant (only)

expectToEqual :: forall a. Eq a => a -> a -> Aff Unit
expectToEqual expected actual = expectToBe true (expected == actual)

testOnly :: Effect ViTest
testOnly = describe "only" do
  test "extracts single variant value - record" do
    let
      variant :: Variant (ok :: { id :: Int, name :: String })
      variant = Variant.inj (Proxy :: _ "ok") { id: 42, name: "Alice" }
      result = only variant
    expectToEqual { id: 42, name: "Alice" } result
