-- EXPECT: name :: String
module CompileFail.ClientMissingBodyField where

import Yoga.Fetch.Om (JSON, POST, Route, client)

type UserApi =
  { createUser :: Route POST "users" { body :: JSON { name :: String, age :: Int } } (ok :: { body :: {} })
  }

api = client @UserApi ""

bad = api.createUser { age: 42 }
