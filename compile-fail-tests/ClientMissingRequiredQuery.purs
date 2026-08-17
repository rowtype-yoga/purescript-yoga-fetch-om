-- EXPECT: id :: Int
module CompileFail.ClientMissingRequiredQuery where

import Justifill (justifill)
import Yoga.Fetch.Om (GET, Required, Route, client, type (:?))

type UserApi =
  { getUser :: Route GET ("users" :? { id :: Required Int, token :: Required String }) {} (ok :: { body :: {} })
  }

api = client @UserApi ""

bad = api.getUser (justifill { token: "secret" })
