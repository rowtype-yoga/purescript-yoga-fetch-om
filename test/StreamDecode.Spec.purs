module StreamDecode.Spec where

import Prelude

import Data.ArrayBuffer.Types (Uint8Array)
import Data.String as String
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (throwException)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Yoga.Fetch.Om.StreamDecode (decodeStream)
import Yoga.Om (Om)
import Yoga.Om as Om
import Yoga.Om.Strom as Strom

foreign import uint8Array :: Array Int -> Uint8Array

runOm :: forall a. Om {} () a -> Aff a
runOm om = Om.runOm {} { exception: \err -> liftEffect (throwException err) } om

spec :: Spec Unit
spec = describe "StreamDecode" do
  it "preserves UTF-8 code points split across chunks" do
    chunks <- runOm $
      Strom.fromArray
        [ uint8Array [ 0xe2 ]
        , uint8Array [ 0x82, 0xac ]
        ]
        # decodeStream @String
        # Strom.runCollect
    String.joinWith "" chunks `shouldEqual` "€"
