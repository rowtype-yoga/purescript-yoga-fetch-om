-- EXPECT: String
module CompileFail.ClientBadBodyType where

import Yoga.Fetch.Om (JSON, POST, Route, client)

type UserApi =
  { createUser :: Route POST "users" { body :: JSON { name :: String } } (ok :: { body :: {} })
  }

api = client @UserApi ""

bad = api.createUser { name: 42 }
