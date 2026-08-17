-- EXPECT: Function
module CompileFail.ClientMissingBodyArgument where

import Yoga.Fetch.Om (JSON, POST, Route, client, type (/), type (:))
import Yoga.Om (Om)

type UserApi =
  { updateUser :: Route POST ("users" / "id" : Int) { body :: JSON { token :: String } } (ok :: { body :: {} })
  }

api :: forall ctx err. { updateUser :: { id :: Int } -> Om { | ctx } err {} }
api = client @UserApi ""
