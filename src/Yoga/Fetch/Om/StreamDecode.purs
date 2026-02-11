module Yoga.Fetch.Om.StreamDecode
  ( class StreamDecode
  , decodeStream
  ) where

import Prelude
import Data.ArrayBuffer.Types (Uint8Array)
import Yoga.Om.Strom (Strom)
import Yoga.Om.Strom as Strom

class StreamDecode a where
  decodeStream :: Strom {} () Uint8Array -> Strom {} () a

instance StreamDecode Uint8Array where
  decodeStream = identity

else instance StreamDecode String where
  decodeStream = Strom.mapStrom decodeUtf8

foreign import decodeUtf8 :: Uint8Array -> String
