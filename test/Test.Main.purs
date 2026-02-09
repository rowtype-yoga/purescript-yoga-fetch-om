module Test.Main where

import Prelude

import BuildUrl.Spec as BuildUrl
import MakeRequest.Spec as MakeRequest
import Polymorphic.Spec as Polymorphic
import SplitParams.Spec as SplitParams
import Variant.Spec as Variant
import Effect (Effect)
import ViTest (ViTest, viTest)

spec :: Effect ViTest
spec = do
  _ <- BuildUrl.testSubstitutePathParams
  _ <- BuildUrl.testAppendQueryParams
  _ <- BuildUrl.testBuildUrl
  _ <- SplitParams.testSplitParams
  _ <- Variant.testOnly
  _ <- MakeRequest.testToHeaders
  Polymorphic.testPolymorphic

main :: ViTest
main = viTest spec
