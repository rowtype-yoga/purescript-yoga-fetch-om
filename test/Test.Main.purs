module Test.Main where

import Prelude

import BuildUrl.Spec as BuildUrl
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
  Variant.testOnly

main :: ViTest
main = viTest spec
