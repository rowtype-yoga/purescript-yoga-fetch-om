module Example where

import Yoga.HTTP.API.Route (GET, POST, DELETE, Route, BearerToken)
import Yoga.HTTP.API.Route.Encoding (JSON)
import Yoga.HTTP.API.Path (Path, type (/), type (:), type (:?))

type User = { id :: UserId, name :: UserName, email :: UserMail }
type CreateUserRequest = { name :: String, email :: UserMail }
type ErrorMessage = { error :: String }
newtype UserId = UserId Int
newtype UserName = UserName String
newtype UserMail = UserMail String
newtype Limit = Limit Int
newtype Offset = Offset Int

type UserAPI =
  { getUser ::
      Route GET
        ("users" / "id" : UserId)
        {}
        ( ok :: { body :: User }
        , notFound :: { body :: ErrorMessage }
        )
  , listUsers ::
      Route GET
        ("users" :? { limit :: Limit, offset :: Offset })
        {}
        ( ok :: { body :: Array User }
        )
  , createUser ::
      Route POST
        "users"
        { body :: JSON CreateUserRequest }
        ( created :: { body :: User }
        , badRequest :: { body :: ErrorMessage }
        )
  , deleteUser ::
      Route DELETE
        ("users" / "id" : UserId)
        {}
        ( noContent :: {}
        , notFound :: { body :: ErrorMessage }
        )
  , createUserAuth ::
      Route POST
        "users"
        { headers :: Record (authorization :: BearerToken), body :: JSON CreateUserRequest }
        ( created :: { body :: User }
        , badRequest :: { body :: ErrorMessage }
        )
  }
