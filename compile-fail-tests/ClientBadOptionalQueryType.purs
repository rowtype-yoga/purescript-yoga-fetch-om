-- EXPECT: limit
module CompileFail.ClientBadOptionalQueryType where

import Justifill (justifill)
import Yoga.Fetch.Om (GET, Route, client, type (:?))

type UserApi =
  { listUsers :: Route GET ("users" :? { limit :: Int, offset :: Int }) {} (ok :: { body :: {} })
  }

api = client @UserApi ""

bad = api.listUsers (justifill { limit: "10" })
