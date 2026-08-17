-- EXPECT: extra
module CompileFail.ClientExtraPathField where

import Yoga.Fetch.Om (GET, Route, client, type (/), type (:))

type UserApi =
  { getUser :: Route GET ("users" / "id" : Int) {} (ok :: { body :: {} })
  }

api = client @UserApi ""

bad = api.getUser { id: 1, extra: true }
