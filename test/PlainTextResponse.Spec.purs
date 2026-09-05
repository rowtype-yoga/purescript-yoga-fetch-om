module PlainTextResponse.Spec where

import Prelude
import Data.String as String

import Effect.Aff (Aff, bracket)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Exception as Exn
import Effect.Ref as Ref
import Foreign (unsafeToForeign)
import Test.Spec (Spec, around, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Yoga.Fastify.Fastify as F
import Yoga.Fastify.Fastify (Port(..), Host(..), RouteURL(..), StatusCode(..))
import Yoga.Fastify.Om (createOmFastify, getOm, postOm)
import Yoga.Fetch.Om (GET, POST, Route, type (/), JSON, PlainText, Streaming, client)
import Yoga.Fetch.Om.Simple as Simple
import Yoga.Om (Om, runOm)
import Yoga.Om.Strom (Strom)
import Yoga.Om.Strom as Strom

type Payload = { value :: Int }
type Result = { doubled :: Int }

type TestAPI =
  { double ::
      Route POST
        ("plain-text-test" / "double")
        { body :: JSON Payload }
        ( ok :: { body :: Result }
        , badRequest :: { body :: PlainText }
        )
  , health :: Route GET ("plain-text-test" / "health") {} (ok :: { body :: PlainText })
  , stream :: Route GET ("plain-text-test" / "stream") {} (ok :: { body :: Streaming String })
  }

api
  :: forall ctx err
   . { double :: Payload -> Om (Record ctx) (badRequest :: String | err) Result
     , health :: Om (Record ctx) err String
     , stream :: Om (Record ctx) err (Strom {} () String)
     }
api = client @TestAPI "http://localhost:44933"

spec :: Spec Unit
spec = around withServer $ describe "String body on error responses" do
  it "200 OK decodes the JSON response body" \_ -> do
    result <- runOm {}
      { exception: \_ -> pure { doubled: -1 }
      , badRequest: \_ -> pure { doubled: -1 }
      }
      (api.double { value: 21 })
    liftAff $ result `shouldEqual` { doubled: 42 }

  it "400 badRequest delivers the raw text body as a String" \serverMode -> do
    liftEffect $ Ref.write "badRequest" serverMode
    captured <- liftEffect $ Ref.new ""
    runOm {}
      { exception: \_ -> pure unit
      , badRequest: \b -> liftEffect $ Ref.write b captured
      }
      (void $ api.double { value: 0 })
    body <- liftEffect $ Ref.read captured
    liftAff $ body `shouldEqual` "invalid value"

  it "Simple.get returns plain text for String responses" \_ -> do
    result <- runOm {}
      { exception: \e -> pure $ "exception: " <> Exn.message e
      , fetchError: \e -> pure $ "fetch error: " <> e.body
      }
      (Simple.get @String "http://localhost:44933/plain-text-test/health" {})
    liftAff $ result `shouldEqual` "healthy"

  it "derived PlainText responses decode to String" \_ -> do
    result <- runOm {}
      { exception: \e -> pure $ "exception: " <> Exn.message e }
      api.health
    liftAff $ result `shouldEqual` "healthy"

  it "derived Streaming responses decode to a usable Strom" \_ -> do
    chunks <- runOm {}
      { exception: \e -> pure [ "exception: " <> Exn.message e ] }
      do
        stream <- api.stream
        Strom.runCollect stream
    liftAff $ String.joinWith "" chunks `shouldEqual` "stream €"

withServer :: forall a. (Ref String -> Aff a) -> Aff a
withServer test = bracket acquire release (\{ serverMode } -> test serverMode)
  where
  acquire = do
    serverMode <- liftEffect $ Ref.new "ok"
    fastify <- liftEffect do
      f <- F.fastify {}
      omApp <- createOmFastify {} f
      postOm (RouteURL "/plain-text-test/double") (handler serverMode) omApp
      getOm (RouteURL "/plain-text-test/health") (plainTextHandler "healthy") omApp
      getOm (RouteURL "/plain-text-test/stream") (plainTextHandler "stream €") omApp
      pure f
    void $ F.listen { port: Port 44933, host: Host "0.0.0.0" } fastify
    pure { fastify, serverMode }

  release { fastify } = F.close fastify

  plainTextHandler text reply = do
    void $ F.status (StatusCode 200) reply # liftEffect
    void $ F.header "content-type" "text/plain" reply # liftEffect
    F.send (unsafeToForeign text) reply # liftAff

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
