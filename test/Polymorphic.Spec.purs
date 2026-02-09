module Polymorphic.Spec where

import Prelude

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Yoga.Fetch.Om (GET, Route, Path, type (/), type (:), client)
import Yoga.Om (Om, ask, runOm)
import ViTest (ViTest, describe, test)
import ViTest.Expect (expectToBe)

type User = { id :: Int, name :: String }

type TestAPI =
  { getUser :: Route GET (Path ("users" / "id" : Int)) {} (ok :: { body :: User })
  }

api
  :: forall ctx err
   . { getUser :: { id :: Int } -> Om ctx err User
     }
api = client @TestAPI "http://localhost:44931"

testPolymorphic :: Effect ViTest
testPolymorphic = describe "polymorphic client" do
  _ <- test "fetches user with { token :: String } context" do
    tokenRef <- liftEffect $ Ref.new ""
    runOm { token: "secret123" } { exception: \_ -> pure unit } do
      ctx <- ask
      liftEffect $ Ref.write ctx.token tokenRef
      user <- api.getUser { id: 42 }
      liftEffect $ Ref.write (ctx.token <> ":" <> user.name) tokenRef
    val <- liftEffect $ Ref.read tokenRef
    expectToBe val "secret123:User 42"

  _ <- test "fetches user with { apiKey :: String, userId :: Int } context" do
    resultRef <- liftEffect $ Ref.new ""
    runOm { apiKey: "mykey", userId: 7 } { exception: \_ -> pure unit } do
      ctx <- ask
      user <- api.getUser { id: ctx.userId }
      liftEffect $ Ref.write (ctx.apiKey <> ":" <> user.name) resultRef
    val <- liftEffect $ Ref.read resultRef
    expectToBe val "mykey:User 7"

  test "same api.getUser works in two different contexts" do
    ref1 <- liftEffect $ Ref.new ""
    ref2 <- liftEffect $ Ref.new ""
    runOm { token: "tok1" } { exception: \_ -> pure unit } do
      ctx <- ask
      user <- api.getUser { id: 1 }
      liftEffect $ Ref.write (ctx.token <> ":" <> user.name) ref1
    runOm { endpoint: "prod", region: "eu" } { exception: \_ -> pure unit } do
      ctx <- ask
      user <- api.getUser { id: 2 }
      liftEffect $ Ref.write (ctx.endpoint <> "-" <> ctx.region <> ":" <> user.name) ref2
    val1 <- liftEffect $ Ref.read ref1
    val2 <- liftEffect $ Ref.read ref2
    expectToBe val1 "tok1:User 1"
    expectToBe val2 "prod-eu:User 2"
