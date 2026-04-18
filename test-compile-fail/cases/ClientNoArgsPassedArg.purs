module Test.CompileFail.ClientNoArgsPassedArg where

import Yoga.Fetch.Om (GET, Route, client)
import Yoga.Om (Om)

type HealthRoute = Route GET "health" {} (ok :: { body :: String })
type TestAPI = { health :: HealthRoute }

api = client @TestAPI "http://localhost:3000"

wrong :: forall ctx err. Om ctx err String
wrong = api.health "should not accept this"
