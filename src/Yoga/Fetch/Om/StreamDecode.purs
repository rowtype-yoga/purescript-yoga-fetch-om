module Yoga.Fetch.Om.StreamDecode
  ( class StreamDecode
  , decodeStream
  ) where

import Prelude

import Control.Monad.Rec.Class (Step(..))
import Data.Array as Array
import Data.ArrayBuffer.Types (Uint8Array)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class (liftEffect)
import Yoga.Om (Om)
import Yoga.Om.Strom (Strom)
import Yoga.Om.Strom as Strom

class StreamDecode a where
  decodeStream :: Strom {} () Uint8Array -> Strom {} () a

instance StreamDecode Uint8Array where
  decodeStream = identity

else instance StreamDecode String where
  decodeStream stream =
    Strom.fromOm (liftEffect newUtf8Decoder) >>= \decoder ->
      decodeUtf8Stream decoder stream

data Utf8Decoder

foreign import newUtf8Decoder :: Effect Utf8Decoder

foreign import decodeUtf8Chunks :: Utf8Decoder -> Array Uint8Array -> Effect (Array String)

foreign import flushUtf8Decoder :: Utf8Decoder -> Effect String

decodeUtf8Stream :: Utf8Decoder -> Strom {} () Uint8Array -> Strom {} () String
decodeUtf8Stream decoder stream = Strom.mkStrom do
  step <- Strom.runStrom stream
  case step of
    Done maybeChunk -> do
      decoded <- decodeChunk decoder maybeChunk
      trailing <- liftEffect $ flushUtf8Decoder decoder
      let
        finalChunk =
          if trailing == "" then decoded
          else Array.snoc decoded trailing
      pure $ Done (nonEmpty finalChunk)
    Loop (maybeChunk /\ next) -> do
      decoded <- decodeChunk decoder maybeChunk
      pure $ Loop (nonEmpty decoded /\ decodeUtf8Stream decoder next)

decodeChunk :: Utf8Decoder -> Maybe (Array Uint8Array) -> Om {} () (Array String)
decodeChunk _ Nothing = pure []
decodeChunk decoder (Just chunk) = liftEffect $ decodeUtf8Chunks decoder chunk

nonEmpty :: forall a. Array a -> Maybe (Array a)
nonEmpty values
  | Array.null values = Nothing
  | otherwise = Just values
