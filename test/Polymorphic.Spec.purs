module Polymorphic.Spec where

import Prelude

import Effect.Aff (bracket)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Test.Spec (Spec, around, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Yoga.Fastify.Fastify as F
import Yoga.Fastify.Fastify (Port(..), Host(..))
import Yoga.Fastify.Om.Route (Handler, Request, handle, respond, handleRoute)
import Yoga.Fetch.Om (GET, Route, type (/), type (:), client)
import Yoga.Om (Om, ask, runOm)

type User = { id :: Int, name :: String }

type TestAPI =
  { getUser :: Route GET ("users" / "id" : Int) (Request {}) (ok :: { body :: User })
  }

api
  :: forall ctx err
   . { getUser :: { id :: Int } -> Om ctx err User
     }
api = client @TestAPI "http://localhost:44931"

spec :: Spec Unit
spec = around withServer $ describe "polymorphic client" do
  it "fetches user with { token :: String } context" \_ ->
    runOm { token: "secret123" } { exception: \_ -> pure unit } do
      ctx <- ask
      user <- api.getUser { id: 42 }
      liftAff $ (ctx.token <> ":" <> user.name) `shouldEqual` "secret123:User 42"

  it "fetches user with { apiKey :: String, userId :: Int } context" \_ ->
    runOm { apiKey: "mykey", userId: 7 } { exception: \_ -> pure unit } do
      ctx <- ask
      user <- api.getUser { id: ctx.userId }
      liftAff $ (ctx.apiKey <> ":" <> user.name) `shouldEqual` "mykey:User 7"

  it "same api.getUser works in two different contexts" \_ -> do
    runOm { token: "tok1" } { exception: \_ -> pure unit } do
      ctx <- ask
      user <- api.getUser { id: 1 }
      liftAff $ (ctx.token <> ":" <> user.name) `shouldEqual` "tok1:User 1"
    runOm { endpoint: "prod", region: "eu" } { exception: \_ -> pure unit } do
      ctx <- ask
      user <- api.getUser { id: 2 }
      liftAff $ (ctx.endpoint <> "-" <> ctx.region <> ":" <> user.name) `shouldEqual` "prod-eu:User 2"

withServer :: forall a. (Unit -> _ a) -> _ a
withServer test = bracket acquire release (\_ -> test unit)
  where
  acquire = do
    fastify <- liftEffect do
      f <- F.fastify {}
      handleRoute getUserHandler f
      pure f
    void $ F.listen { port: Port 44931, host: Host "0.0.0.0" } fastify
    pure fastify

  release = F.close

type GetUserRoute = Route GET ("users" / "id" : Int) (Request {}) (ok :: { body :: User })

getUserHandler :: Handler GetUserRoute
getUserHandler = handle do
  { path } <- ask
  respond @"ok" { id: path.id, name: "User " <> show path.id }
