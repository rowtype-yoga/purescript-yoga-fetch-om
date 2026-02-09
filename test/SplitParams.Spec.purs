module SplitParams.Spec where

import Prelude

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Yoga.Fetch.Om.SplitParams (splitParams)

spec :: Spec Unit
spec = describe "splitParams" do
  it "splits path and query params" do
    let
      allParams = { id: 42, limit: 10, offset: 20 }
      result =
        splitParams allParams
          :: { path :: { id :: Int }
             , query :: { limit :: Int, offset :: Int }
             , body :: {}
             }
    result.path.id `shouldEqual` 42
    result.query.limit `shouldEqual` 10
    result.query.offset `shouldEqual` 20
    result.body `shouldEqual` {}

  it "splits path and body params" do
    let
      allParams = { id: 42, name: "Alice", email: "alice@example.com" }
      result =
        splitParams allParams
          :: { path :: { id :: Int }
             , query :: {}
             , body :: { name :: String, email :: String }
             }
    result.path.id `shouldEqual` 42
    result.query `shouldEqual` {}
    result.body.name `shouldEqual` "Alice"
    result.body.email `shouldEqual` "alice@example.com"

  it "splits all three param types" do
    let
      allParams =
        { id: 42
        , limit: 10
        , name: "Bob"
        }
      result =
        splitParams allParams
          :: { path :: { id :: Int }
             , query :: { limit :: Int }
             , body :: { name :: String }
             }
    result.path.id `shouldEqual` 42
    result.query.limit `shouldEqual` 10
    result.body.name `shouldEqual` "Bob"

  it "handles empty splits" do
    let
      allParams = { id: 42 }
      result =
        splitParams allParams
          :: { path :: { id :: Int }
             , query :: {}
             , body :: {}
             }
    result.path.id `shouldEqual` 42
    result.query `shouldEqual` {}
    result.body `shouldEqual` {}
