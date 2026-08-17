-- EXPECT: Function
module CompileFail.ClientMissingHeaderArgument where

import Yoga.Fetch.Om (GET, Route, client, type (/), type (:))
import Yoga.HTTP.API.Route (BearerToken)
import Yoga.Om (Om)

type UserApi =
  { getUser :: Route GET ("users" / "id" : Int) { headers :: { authorization :: BearerToken } } (ok :: { body :: {} })
  }

api :: forall ctx err. { getUser :: { id :: Int } -> Om { | ctx } err {} }
api = client @UserApi ""
