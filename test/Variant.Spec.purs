module Variant.Spec where

import Prelude

import Data.Variant (Variant)
import Data.Variant as Variant
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))
import Yoga.Fetch.Om.Variant (only)

spec :: Spec Unit
spec = describe "only" do
  it "extracts single variant value - record" do
    let
      variant :: Variant (ok :: { id :: Int, name :: String })
      variant = Variant.inj (Proxy :: _ "ok") { id: 42, name: "Alice" }
      result = only variant
    result `shouldEqual` { id: 42, name: "Alice" }
