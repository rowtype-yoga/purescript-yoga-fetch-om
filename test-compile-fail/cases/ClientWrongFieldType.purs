module Test.CompileFail.ClientWrongFieldType where

import Yoga.Fetch.Om (GET, Route, type (/), type (:), client)
import Yoga.Om (Om)

type GetUserRoute = Route GET ("users" / "id" : Int) {} (ok :: { body :: { id :: Int, name :: String } })
type TestAPI = { getUser :: GetUserRoute }

api = client @TestAPI "http://localhost:3000"

wrong :: forall ctx err. Om ctx err { id :: Int, name :: String }
wrong = api.getUser { id: "not-an-int" }
