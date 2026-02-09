module TestServer where

import Prelude

import Data.Int as Int
import Data.Maybe (fromMaybe)
import Effect (Effect)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Foreign (unsafeToForeign)
import Foreign.Object as FObject
import Promise (Promise)
import Promise.Aff (fromAff)
import Yoga.Fastify.Fastify as F
import Yoga.Fastify.Fastify (Fastify, Port(..), Host(..), RouteURL(..), StatusCode(..))
import Yoga.Fastify.Om (createOmFastify, getOm, requestParams)

mkServer :: Effect Fastify
mkServer = do
  fastify <- F.fastify {}
  omApp <- createOmFastify {} fastify
  getOm (RouteURL "/users/:id") handleGetUser omApp
  pure fastify
  where
  handleGetUser reply = do
    params <- requestParams
    let idStr = fromMaybe "0" (FObject.lookup "id" params)
    let id = fromMaybe 0 (Int.fromString idStr)
    let body = unsafeToForeign { id, name: "User " <> idStr }
    void $ F.status (StatusCode 200) reply # liftEffect
    void $ F.header "content-type" "application/json" reply # liftEffect
    F.sendJson body reply # liftAff

listenServer :: Fastify -> Effect (Promise String)
listenServer fastify = fromAff $ F.listen { port: Port 44931, host: Host "0.0.0.0" } fastify

closeServer :: Fastify -> Effect (Promise Unit)
closeServer fastify = fromAff $ F.close fastify
