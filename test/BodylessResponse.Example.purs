-- Compile-time test: bodyless responses (e.g. 304, 401) can use {} without
-- wrapping in { body :: {} }.
module BodylessResponse.Example where

import Prelude

import Yoga.Fetch.Om (GET, Route, type (/), client)
import Yoga.Om (Om)

type Resource = { id :: Int, name :: String }

type ResourceAPI =
  { getResource ::
      Route GET
        ("resources" / "id")
        {}
        ( ok           :: { body :: Resource }
        , notModified  :: {}
        , unauthorized :: {}
        )
  }

-- This must typecheck: the derived client exposes notModified/unauthorized as Unit errors.
api
  :: forall ctx err
   . { getResource :: Om (Record ctx) (notModified :: Unit, unauthorized :: Unit | err) Resource }
api = client @ResourceAPI "https://api.example.com"
