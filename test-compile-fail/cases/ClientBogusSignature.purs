module Test.CompileFail.ClientBogusSignature where

import Yoga.Fetch.Om (GET, Route, type (/), type (:), client)

type GetUserRoute = Route GET ("users" / "id" : Int) {} (ok :: { body :: { id :: Int, name :: String } })
type TestAPI = { getUser :: GetUserRoute }

wrong :: { getUser :: String -> Int }
wrong = client @TestAPI "http://localhost:3000"
