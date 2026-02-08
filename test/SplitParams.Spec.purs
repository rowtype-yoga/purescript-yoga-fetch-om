module SplitParams.Spec where

import Prelude

import Effect (Effect)
import Effect.Aff (Aff)
import Yoga.Fetch.Om.SplitParams (splitParams)
import ViTest (ViTest, describe, test)
import ViTest.Expect (expectToBe)

expectToEqual :: forall a. Eq a => a -> a -> Aff Unit
expectToEqual expected actual = expectToBe true (expected == actual)

--------------------------------------------------------------------------------
-- SplitParams Tests
--------------------------------------------------------------------------------

testSplitParams :: Effect ViTest
testSplitParams = describe "splitParams" $ do
  _ <- test "splits path and query params" do
    let
      allParams = { id: 42, limit: 10, offset: 20 }
      result =
        splitParams allParams
          :: { path :: { id :: Int }
             , query :: { limit :: Int, offset :: Int }
             , body :: {}
             }
    expectToEqual 42 result.path.id
    expectToEqual 10 result.query.limit
    expectToEqual 20 result.query.offset
    expectToEqual {} result.body

  _ <- test "splits path and body params" do
    let
      allParams = { id: 42, name: "Alice", email: "alice@example.com" }
      result =
        splitParams allParams
          :: { path :: { id :: Int }
             , query :: {}
             , body :: { name :: String, email :: String }
             }
    expectToEqual 42 result.path.id
    expectToEqual {} result.query
    expectToEqual "Alice" result.body.name
    expectToEqual "alice@example.com" result.body.email

  _ <- test "splits all three param types" do
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
    expectToEqual 42 result.path.id
    expectToEqual 10 result.query.limit
    expectToEqual "Bob" result.body.name

  test "handles empty splits" do
    let
      allParams = { id: 42 }
      result =
        splitParams allParams
          :: { path :: { id :: Int }
             , query :: {}
             , body :: {}
             }
    expectToEqual 42 result.path.id
    expectToEqual {} result.query
    expectToEqual {} result.body
