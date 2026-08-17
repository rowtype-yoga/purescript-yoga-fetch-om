-- EXPECT: Int
module CompileFail.ClientBadPathType where

import Yoga.Fetch.Om (GET, Route, client, type (/), type (:))

type UserApi =
  { getUser :: Route GET ("users" / "id" : Int) {} (ok :: { body :: {} })
  }

api = client @UserApi ""

bad = api.getUser { id: "1" }
