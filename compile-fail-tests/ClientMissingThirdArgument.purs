-- EXPECT: Function
module CompileFail.ClientMissingThirdArgument where

import Yoga.Fetch.Om (JSON, POST, Route, client, type (/), type (:))
import Yoga.HTTP.API.Route (BearerToken)
import Yoga.Om (Om)

type UserApi =
  { updateUser :: Route POST ("users" / "id" : Int) { headers :: { authorization :: BearerToken }, body :: JSON { token :: String } } (ok :: { body :: {} })
  }

api :: forall ctx err. { updateUser :: { id :: Int } -> { authorization :: BearerToken } -> Om { | ctx } err {} }
api = client @UserApi ""
