module BuildUrl.Spec where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Type.Proxy (Proxy(..))
import Yoga.Fetch.Om.BuildUrl (buildUrl, substitutePathParams, appendQueryParams)
import Yoga.HTTP.API.Path (Path, type (/), type (:))
import ViTest (ViTest, describe, test)
import ViTest.Expect (expectToBe)

expectToEqual :: forall a. Eq a => a -> a -> Aff Unit
expectToEqual expected actual = expectToBe true (expected == actual)

--------------------------------------------------------------------------------
-- SubstitutePathParams Tests
--------------------------------------------------------------------------------

testSubstitutePathParams :: Effect ViTest
testSubstitutePathParams = describe "substitutePathParams" $ do
  _ <- test "substitutes single path param" do
    let
      result = substitutePathParams (Proxy :: _ (id :: Int)) { id: 42 } "/users/:id"
    expectToEqual "/users/42" result

  _ <- test "substitutes multiple path params" do
    let
      result = substitutePathParams (Proxy :: _ (userId :: Int, postId :: Int))
        { userId: 1, postId: 99 }
        "/users/:userId/posts/:postId"
    expectToEqual "/users/1/posts/99" result

  _ <- test "handles String params" do
    let
      result = substitutePathParams (Proxy :: _ (slug :: String))
        { slug: "hello-world" }
        "/posts/:slug"
    expectToEqual "/posts/hello-world" result

  test "handles empty params" do
    let
      result = substitutePathParams (Proxy :: _ ()) {} "/users"
    expectToEqual "/users" result

--------------------------------------------------------------------------------
-- AppendQueryParams Tests
--------------------------------------------------------------------------------

testAppendQueryParams :: Effect ViTest
testAppendQueryParams = describe "appendQueryParams" $ do
  _ <- test "appends single query param" do
    let
      result = appendQueryParams (Proxy :: _ (limit :: Int)) { limit: 10 } "/users"
    expectToEqual "/users?limit=10" result

  _ <- test "appends multiple query params" do
    let
      result = appendQueryParams (Proxy :: _ (limit :: Int, offset :: Int))
        { limit: 10, offset: 20 }
        "/users"
    expectToEqual "/users?limit=10&offset=20" result

  _ <- test "handles Maybe params - Just" do
    let
      result = appendQueryParams (Proxy :: _ (limit :: Maybe Int))
        { limit: Just 10 }
        "/users"
    expectToEqual "/users?limit=10" result

  _ <- test "handles Maybe params - Nothing" do
    let
      result = appendQueryParams (Proxy :: _ (limit :: Maybe Int))
        { limit: Nothing }
        "/users"
    expectToEqual "/users" result

  _ <- test "handles mixed Maybe and required params" do
    let
      result = appendQueryParams (Proxy :: _ (limit :: Maybe Int, offset :: Int))
        { limit: Nothing, offset: 20 }
        "/users"
    expectToEqual "/users?offset=20" result

  test "handles empty query params" do
    let
      result = appendQueryParams (Proxy :: _ ()) {} "/users"
    expectToEqual "/users" result

--------------------------------------------------------------------------------
-- BuildUrl Tests
--------------------------------------------------------------------------------

testBuildUrl :: Effect ViTest
testBuildUrl = describe "buildUrl" $ do
  _ <- test "builds URL with path params only" do
    let
      result = buildUrl
        "https://api.example.com"
        (Proxy :: _ (Path ("users" / "id" : Int)))
        { id: 42 }
        {}
    expectToEqual "https://api.example.com/users/42" result

  test "builds URL with query params only" do
    let
      result = buildUrl
        "https://api.example.com"
        (Proxy :: _ (Path "users"))
        {}
        { limit: 10, offset: 20 }
    expectToEqual "https://api.example.com/users?limit=10&offset=20" result
