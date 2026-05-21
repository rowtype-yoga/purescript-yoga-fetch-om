module PlainTextResponse.Spec where

import Prelude

import Effect.Aff (Aff, bracket)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Foreign (unsafeToForeign)
import Test.Spec (Spec, around, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Yoga.Fastify.Fastify as F
import Yoga.Fastify.Fastify (Port(..), Host(..), RouteURL(..), StatusCode(..))
import Yoga.Fastify.Om (createOmFastify, postOm)
import Yoga.Fetch.Om (POST, Route, type (/), JSON, client)
import Yoga.Om (Om, runOm)

type Payload = { value :: Int }
type Result  = { doubled :: Int }

type TestAPI =
  { double ::
      Route POST
        ("plain-text-test" / "double")
        { body :: JSON Payload }
        ( ok         :: { body :: Result }
        , badRequest :: { body :: String }
        )
  }

api :: { double :: forall ctx err. Payload -> Om ctx (badRequest :: String | err) Result }
api = client @TestAPI "http://localhost:44933"

spec :: Spec Unit
spec = around withServer $ describe "String body on error responses" do
  it "200 OK decodes the JSON response body" \_ -> do
    result <- runOm {}
      { exception:  \_ -> pure { doubled: -1 }
      , badRequest: \_ -> pure { doubled: -1 }
      }
      (api.double { value: 21 })
    liftAff $ result `shouldEqual` { doubled: 42 }

  it "400 badRequest delivers the raw text body as a String" \serverMode -> do
    liftEffect $ Ref.write "badRequest" serverMode
    captured <- liftEffect $ Ref.new ""
    runOm {}
      { exception:  \_ -> pure unit
      , badRequest: \b -> liftEffect $ Ref.write b captured
      }
      (void $ api.double { value: 0 })
    body <- liftEffect $ Ref.read captured
    liftAff $ body `shouldEqual` "invalid value"

withServer :: forall a. (Ref String -> Aff a) -> Aff a
withServer test = bracket acquire release (\{ serverMode } -> test serverMode)
  where
  acquire = do
    serverMode <- liftEffect $ Ref.new "ok"
    fastify <- liftEffect do
      f <- F.fastify {}
      omApp <- createOmFastify {} f
      postOm (RouteURL "/plain-text-test/double") (handler serverMode) omApp
      pure f
    void $ F.listen { port: Port 44933, host: Host "0.0.0.0" } fastify
    pure { fastify, serverMode }

  release { fastify } = F.close fastify

  handler serverMode reply = do
    mode <- liftEffect $ Ref.read serverMode
    case mode of
      "badRequest" -> do
        void $ F.status (StatusCode 400) reply # liftEffect
        void $ F.header "content-type" "text/plain" reply # liftEffect
        F.send (unsafeToForeign "invalid value") reply # liftAff
      _ -> do
        void $ F.status (StatusCode 200) reply # liftEffect
        void $ F.header "content-type" "application/json" reply # liftEffect
        F.sendJson (unsafeToForeign { doubled: 42 }) reply # liftAff
