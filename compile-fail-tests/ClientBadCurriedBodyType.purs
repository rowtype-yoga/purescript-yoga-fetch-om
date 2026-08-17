-- EXPECT: String
module CompileFail.ClientBadCurriedBodyType where

import Yoga.Fetch.Om (JSON, POST, Route, client, type (/), type (:))

type UserApi =
  { updateUser :: Route POST ("users" / "id" : Int) { body :: JSON { name :: String } } (ok :: { body :: {} })
  }

api = client @UserApi ""

bad = api.updateUser { id: 1 } { name: 42 }
