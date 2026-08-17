-- EXPECT: extra
module CompileFail.ClientExtraHeaderField where

import Yoga.Fetch.Om (GET, Route, client)
import Yoga.HTTP.API.Route (BearerToken(..))

type UserApi =
  { me :: Route GET "me" { headers :: { authorization :: BearerToken } } (ok :: { body :: {} })
  }

api = client @UserApi ""

bad = api.me { authorization: BearerToken "secret", extra: true }
