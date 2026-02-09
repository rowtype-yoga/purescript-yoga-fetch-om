module BuildUrl.Spec where

import Prelude

import Data.Maybe (Maybe(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))
import Yoga.Fetch.Om.BuildUrl (buildUrl, substitutePathParams, appendQueryParams)
import Yoga.HTTP.API.Path (Path, type (/), type (:))

--------------------------------------------------------------------------------
-- SubstitutePathParams Tests
--------------------------------------------------------------------------------

spec :: Spec Unit
spec = do
  describe "substitutePathParams" do
    it "substitutes single path param" do
      let result = substitutePathParams @(id :: Int) { id: 42 } "/users/:id"
      result `shouldEqual` "/users/42"

    it "substitutes multiple path params" do
      let
        result = substitutePathParams @(userId :: Int, postId :: Int)
          { userId: 1, postId: 99 }
          "/users/:userId/posts/:postId"
      result `shouldEqual` "/users/1/posts/99"

    it "handles String params" do
      let
        result = substitutePathParams @(slug :: String)
          { slug: "hello-world" }
          "/posts/:slug"
      result `shouldEqual` "/posts/hello-world"

    it "handles empty params" do
      let result = substitutePathParams @() {} "/users"
      result `shouldEqual` "/users"

  describe "appendQueryParams" do
    it "appends single query param" do
      let result = appendQueryParams @(limit :: Int) { limit: 10 } "/users"
      result `shouldEqual` "/users?limit=10"

    it "appends multiple query params" do
      let
        result = appendQueryParams @(limit :: Int, offset :: Int)
          { limit: 10, offset: 20 }
          "/users"
      result `shouldEqual` "/users?limit=10&offset=20"

    it "handles Maybe params - Just" do
      let
        result = appendQueryParams @(limit :: Maybe Int)
          { limit: Just 10 }
          "/users"
      result `shouldEqual` "/users?limit=10"

    it "handles Maybe params - Nothing" do
      let
        result = appendQueryParams @(limit :: Maybe Int)
          { limit: Nothing }
          "/users"
      result `shouldEqual` "/users"

    it "handles mixed Maybe and required params" do
      let
        result = appendQueryParams @(limit :: Maybe Int, offset :: Int)
          { limit: Nothing, offset: 20 }
          "/users"
      result `shouldEqual` "/users?offset=20"

    it "handles empty query params" do
      let result = appendQueryParams @() {} "/users"
      result `shouldEqual` "/users"

    it "URL-encodes special characters in values" do
      let result = appendQueryParams @(search :: String) { search: "hello world&more=yes" } "/users"
      result `shouldEqual` "/users?search=hello%20world%26more%3Dyes"

  describe "buildUrl" do
    it "builds URL with path params only" do
      let
        result = buildUrl
          "https://api.example.com"
          (Proxy :: _ (Path ("users" / "id" : Int)))
          { id: 42 }
          {}
      result `shouldEqual` "https://api.example.com/users/42"

    it "builds URL with query params only" do
      let
        result = buildUrl
          "https://api.example.com"
          (Proxy :: _ (Path "users"))
          {}
          { limit: 10, offset: 20 }
      result `shouldEqual` "https://api.example.com/users?limit=10&offset=20"
