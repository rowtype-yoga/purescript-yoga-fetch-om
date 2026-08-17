module BodylessResponse.Spec where

import Prelude

import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Aff (Aff, bracket)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Foreign (unsafeToForeign)
import Test.Spec (Spec, around, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Yoga.Fastify.Fastify as F
import Yoga.Fastify.Fastify (Port(..), Host(..), RouteURL(..), StatusCode(..))
import Yoga.Fastify.Om (createOmFastify, deleteOm, getOm)
import Yoga.Fetch.Om (DELETE, GET, Route, type (/), type (:), client)
import Yoga.Om (runOm)

type Resource = { id :: Int, name :: String }

type ResourceAPI =
  { getResource ::
      Route GET
        ("bodyless-test" / "resource")
        {}
        ( ok           :: { body :: Resource }
        , notModified  :: {}
        , unauthorized :: {}
        )
  , deleteUser ::
      Route DELETE
        ("bodyless-test" / "users" / "id" : Int)
        {}
        ( noContent :: {}
        , notFound :: { body :: { error :: String } }
        )
  }

api = client @ResourceAPI "http://localhost:44932"

spec :: Spec Unit
spec = around withServer $ describe "bodyless responses (notModified / unauthorized)" do
  it "304 Not Modified triggers the notModified handler" \serverMode -> do
    liftEffect $ Ref.write "notModified" serverMode
    triggered <- liftEffect $ Ref.new false
    runOm {}
      { exception: \_ -> pure unit
      , notModified: \_ -> liftEffect $ Ref.write true triggered
      , unauthorized: \_ -> pure unit
      }
      (void api.getResource)
    result <- liftEffect $ Ref.read triggered
    liftAff $ result `shouldEqual` true

  it "401 Unauthorized triggers the unauthorized handler" \serverMode -> do
    liftEffect $ Ref.write "unauthorized" serverMode
    triggered <- liftEffect $ Ref.new false
    runOm {}
      { exception: \_ -> pure unit
      , notModified: \_ -> pure unit
      , unauthorized: \_ -> liftEffect $ Ref.write true triggered
      }
      (void api.getResource)
    result <- liftEffect $ Ref.read triggered
    liftAff $ result `shouldEqual` true

  it "200 OK returns the resource body" \serverMode -> do
    liftEffect $ Ref.write "ok" serverMode
    resource <- runOm {}
      { exception: \_ -> pure { id: -1, name: "" }
      , notModified: \_ -> pure { id: -1, name: "" }
      , unauthorized: \_ -> pure { id: -1, name: "" }
      }
      api.getResource
    liftAff $ resource `shouldEqual` { id: 1, name: "test" }

  it "204 No Content returns Unit for noContent shorthand" \serverMode -> do
    liftEffect $ Ref.write "deleted" serverMode
    result <- runOm {}
      { exception: \_ -> pure false
      , notFound: \_ -> pure false
      }
      (api.deleteUser { id: 1 } $> true)
    liftAff $ result `shouldEqual` true

withServer :: forall a. (Ref String -> Aff a) -> Aff a
withServer test = bracket acquire release (\{ serverMode } -> test serverMode)
  where
  acquire = do
    serverMode <- liftEffect $ Ref.new "ok"
    fastify <- liftEffect do
      f <- F.fastify {}
      omApp <- createOmFastify {} f
      getOm (RouteURL "/bodyless-test/resource") (handler serverMode) omApp
      deleteOm (RouteURL "/bodyless-test/users/:id") (deleteHandler serverMode) omApp
      pure f
    void $ F.listen { port: Port 44932, host: Host "0.0.0.0" } fastify
    pure { fastify, serverMode }

  release { fastify } = F.close fastify

  handler serverMode reply = do
    mode <- liftEffect $ Ref.read serverMode
    case mode of
      "notModified" -> do
        void $ F.status (StatusCode 304) reply # liftEffect
        F.send (unsafeToForeign "") reply # liftAff
      "unauthorized" -> do
        void $ F.status (StatusCode 401) reply # liftEffect
        F.send (unsafeToForeign "") reply # liftAff
      _ -> do
        void $ F.status (StatusCode 200) reply # liftEffect
        void $ F.header "content-type" "application/json" reply # liftEffect
        F.sendJson (unsafeToForeign { id: 1, name: "test" }) reply # liftAff

  deleteHandler serverMode reply = do
    mode <- liftEffect $ Ref.read serverMode
    case mode of
      "deleted" -> do
        void $ F.status (StatusCode 204) reply # liftEffect
        F.send (unsafeToForeign "") reply # liftAff
      _ -> do
        void $ F.status (StatusCode 404) reply # liftEffect
        void $ F.header "content-type" "application/json" reply # liftEffect
        F.sendJson (unsafeToForeign { error: "Missing user" }) reply # liftAff
