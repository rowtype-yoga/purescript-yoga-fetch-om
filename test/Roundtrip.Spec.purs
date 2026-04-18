module Roundtrip.Spec where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff (bracket)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Test.Spec (Spec, around, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Yoga.Fastify.Fastify as F
import Yoga.Fastify.Fastify (Port(..), Host(..))
import Yoga.Fastify.Om.API (registerAPI)
import Yoga.Fastify.Om.Route (Handler, handle, respond, reject, BearerToken(..))
import Yoga.Fetch.Om (GET, POST, PUT, DELETE, Route, JSON, type (/), type (:), type (:?), client)
import Yoga.Om (Om, ask, handleErrors, runOm)

-- Types

type User = { id :: Int, name :: String, email :: String }
type CreateUserReq = { name :: String, email :: String }
type ErrorMsg = { error :: String }

-- Route definitions shared by server and client

type GetUserRoute = Route GET
  ("users" / "id" : Int)
  {}
  ( ok :: { body :: User }
  , notFound :: { body :: ErrorMsg }
  )

type ListUsersRoute = Route GET
  ("users" :? { limit :: Int, offset :: Int })
  {}
  (ok :: { body :: Array User })

type CreateUserRoute = Route POST
  "users"
  { body :: JSON CreateUserReq }
  ( created :: { body :: User }
  , badRequest :: { body :: ErrorMsg }
  )

type UpdateUserRoute = Route PUT
  ("users" / "id" : Int)
  { body :: JSON CreateUserReq }
  ( ok :: { body :: User }
  , notFound :: { body :: ErrorMsg }
  )

type DeleteUserRoute = Route DELETE
  ("users" / "id" : Int)
  {}
  ( ok :: { body :: {} }
  , notFound :: { body :: ErrorMsg }
  )

type AuthRoute = Route GET
  "me"
  { headers :: { authorization :: BearerToken } }
  ( ok :: { body :: User }
  , unauthorized :: { body :: ErrorMsg }
  )

type SearchRoute = Route GET
  ("search" :? { active :: Boolean, minScore :: Number })
  {}
  (ok :: { body :: { active :: Boolean, minScore :: Number } })

type API =
  { getUser :: GetUserRoute
  , listUsers :: ListUsersRoute
  , createUser :: CreateUserRoute
  , updateUser :: UpdateUserRoute
  , deleteUser :: DeleteUserRoute
  , me :: AuthRoute
  , search :: SearchRoute
  }

api = client @API "http://localhost:44932"

-- Handlers

getUserHandler :: Handler GetUserRoute ()
getUserHandler = handle do
  { path } <- ask
  if path.id == 42
    then respond @"ok" { id: 42, name: "Alice", email: "alice@test.com" }
    else reject @"notFound" { error: "User " <> show path.id <> " not found" }

listUsersHandler :: Handler ListUsersRoute ()
listUsersHandler = handle do
  { query } <- ask
  let users = [ { id: 1, name: "Alice", email: "a@t.com" }
              , { id: 2, name: "Bob", email: "b@t.com" }
              , { id: 3, name: "Charlie", email: "c@t.com" }
              ]
  let limited = case query.limit of
        Just l -> take l users
        Nothing -> users
  respond @"ok" limited
  where
  take n xs = if n <= 0 then [] else case xs of
    [] -> []
    [a] -> [a]
    [a, _] | n == 1 -> [a]
    [a, b] -> [a, b]
    _ -> xs

createUserHandler :: Handler CreateUserRoute ()
createUserHandler = handle do
  { body } <- ask
  if body.name == ""
    then reject @"badRequest" { error: "Name required" }
    else respond @"created" { id: 99, name: body.name, email: body.email }

updateUserHandler :: Handler UpdateUserRoute ()
updateUserHandler = handle do
  { path, body } <- ask
  if path.id /= 42
    then reject @"notFound" { error: "Not found" }
    else respond @"ok" { id: path.id, name: body.name, email: body.email }

deleteUserHandler :: Handler DeleteUserRoute ()
deleteUserHandler = handle do
  { path } <- ask
  if path.id /= 42
    then reject @"notFound" { error: "Not found" }
    else respond @"ok" {}

meHandler :: Handler AuthRoute ()
meHandler = handle do
  { headers } <- ask
  case headers.authorization of
    BearerToken "secret" -> respond @"ok" { id: 1, name: "Admin", email: "admin@test.com" }
    _ -> reject @"unauthorized" { error: "Invalid token" }

searchHandler :: Handler SearchRoute ()
searchHandler = handle do
  { query } <- ask
  let active = case query.active of
        Just a -> a
        Nothing -> false
  let minScore = case query.minScore of
        Just s -> s
        Nothing -> 0.0
  respond @"ok" { active, minScore }

-- Test suite

spec :: Spec Unit
spec = around withServer $ describe "server ↔ client roundtrip" do

  describe "GET with path params" do
    it "returns user for valid id" \_ ->
      runOm {} { exception: \_ -> pure unit } do
        user <- api.getUser { id: 42 }
          # handleErrors { notFound: \e -> pure { id: 0, name: e.error, email: "" } }
        liftAff $ user.name `shouldEqual` "Alice"

    it "returns notFound for invalid id" \_ ->
      runOm {} { exception: \_ -> pure unit } do
        user <- api.getUser { id: 999 }
          # handleErrors { notFound: \e -> pure { id: 0, name: e.error, email: "" } }
        liftAff $ user.name `shouldEqual` "User 999 not found"

  describe "GET with query params" do
    it "passes query params correctly" \_ ->
      runOm {} { exception: \_ -> pure unit } do
        users <- api.listUsers { limit: 1 }
        liftAff $ (map _.name users) `shouldEqual` ["Alice"]

    it "handles no query params" \_ ->
      runOm {} { exception: \_ -> pure unit } do
        users <- api.listUsers {}
        liftAff $ (map _.name users) `shouldEqual` ["Alice", "Bob", "Charlie"]

  describe "POST with JSON body" do
    it "sends and receives JSON body" \_ ->
      runOm {} { exception: \_ -> pure unit } do
        user <- api.createUser { name: "Dave", email: "d@t.com" }
          # handleErrors { badRequest: \e -> pure { id: 0, name: e.error, email: "" } }
        liftAff do
          user.name `shouldEqual` "Dave"
          user.email `shouldEqual` "d@t.com"
          user.id `shouldEqual` 99

    it "rejects with badRequest for empty name" \_ ->
      runOm {} { exception: \_ -> pure unit } do
        user <- api.createUser { name: "", email: "e@t.com" }
          # handleErrors { badRequest: \e -> pure { id: 0, name: e.error, email: "" } }
        liftAff $ user.name `shouldEqual` "Name required"

  describe "PUT with path params and JSON body" do
    it "sends path params and body together" \_ ->
      runOm {} { exception: \_ -> pure unit } do
        user <- api.updateUser { id: 42 } { name: "Updated", email: "u@t.com" }
          # handleErrors { notFound: \e -> pure { id: 0, name: e.error, email: "" } }
        liftAff do
          user.id `shouldEqual` 42
          user.name `shouldEqual` "Updated"

  describe "DELETE with path params" do
    it "deletes successfully" \_ ->
      runOm {} { exception: \_ -> pure unit } do
        result <- api.deleteUser { id: 42 }
          # handleErrors { notFound: \_ -> pure {} }
        liftAff $ result `shouldEqual` {}

    it "returns notFound for wrong id" \_ ->
      runOm {} { exception: \_ -> pure unit } do
        result <- api.deleteUser { id: 1 }
          # handleErrors { notFound: \_ -> pure {} }
        liftAff $ result `shouldEqual` {}

  describe "GET with authorization header" do
    it "passes bearer token in header" \_ ->
      runOm {} { exception: \_ -> pure unit } do
        user <- api.me { authorization: BearerToken "secret" }
          # handleErrors { unauthorized: \e -> pure { id: 0, name: e.error, email: "" } }
        liftAff $ user.name `shouldEqual` "Admin"

    it "rejects unauthorized" \_ ->
      runOm {} { exception: \_ -> pure unit } do
        user <- api.me { authorization: BearerToken "wrong" }
          # handleErrors { unauthorized: \e -> pure { id: 0, name: e.error, email: "" } }
        liftAff $ user.name `shouldEqual` "Invalid token"

  describe "GET with Boolean and Number query params" do
    it "passes Boolean and Number query params" \_ ->
      runOm {} { exception: \_ -> pure unit } do
        result <- api.search { active: true, minScore: 3.14 }
        liftAff do
          result.active `shouldEqual` true
          result.minScore `shouldEqual` 3.14

    it "handles missing optional query params" \_ ->
      runOm {} { exception: \_ -> pure unit } do
        result <- api.search {}
        liftAff do
          result.active `shouldEqual` false
          result.minScore `shouldEqual` 0.0

  describe "partial query params — no wrapper needed" do
    it "passes subset of query params directly" \_ ->
      runOm {} { exception: \_ -> pure unit } do
        users <- api.listUsers { limit: 1 }
        liftAff $ (map _.name users) `shouldEqual` ["Alice"]

    it "passes all query params directly" \_ ->
      runOm {} { exception: \_ -> pure unit } do
        users <- api.listUsers { limit: 2, offset: 0 }
        liftAff $ (map _.name users) `shouldEqual` ["Alice", "Bob"]

    it "passes empty record for all defaults" \_ ->
      runOm {} { exception: \_ -> pure unit } do
        users <- api.listUsers {}
        liftAff $ (map _.name users) `shouldEqual` ["Alice", "Bob", "Charlie"]

    it "passes partial Boolean query param" \_ ->
      runOm {} { exception: \_ -> pure unit } do
        result <- api.search { active: true }
        liftAff do
          result.active `shouldEqual` true
          result.minScore `shouldEqual` 0.0

-- Server setup

withServer :: forall a. (Unit -> _ a) -> _ a
withServer test = bracket acquire release (\_ -> test unit)
  where
  acquire = do
    fastify <- liftEffect do
      f <- F.fastify {}
      registerAPI @API
        { getUser: getUserHandler
        , listUsers: listUsersHandler
        , createUser: createUserHandler
        , updateUser: updateUserHandler
        , deleteUser: deleteUserHandler
        , me: meHandler
        , search: searchHandler
        }
        f
      pure f
    void $ F.listen { port: Port 44932, host: Host "0.0.0.0" } fastify
    pure fastify

  release = F.close
