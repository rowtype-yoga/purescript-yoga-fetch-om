module Example where

import Yoga.HTTP.API.Route (GET, POST, DELETE, Route)
import Yoga.HTTP.API.Route.Encoding (JSON)
import Yoga.HTTP.API.Path (Path, type (/), type (:), type (:?))

type User = { id :: Int, name :: String, email :: String }
type CreateUserRequest = { name :: String, email :: String }
type ErrorMessage = { error :: String }

type UserAPI =
  { getUser ::
      Route GET
        (Path ("users" / "id" : Int))
        {}
        ( ok :: { body :: User }
        , notFound :: { body :: ErrorMessage }
        )
  , listUsers ::
      Route GET
        (Path "users" :? { limit :: Int, offset :: Int })
        {}
        ( ok :: { body :: Array User }
        )
  , createUser ::
      Route POST
        (Path "users")
        { body :: JSON CreateUserRequest }
        ( created :: { body :: User }
        , badRequest :: { body :: ErrorMessage }
        )
  , deleteUser ::
      Route DELETE
        (Path ("users" / "id" : Int))
        {}
        ( noContent :: { body :: {} }
        , notFound :: { body :: ErrorMessage }
        )
  , createUserAuth ::
      Route POST
        (Path "users")
        { headers :: Record (authorization :: String), body :: JSON CreateUserRequest }
        ( created :: { body :: User }
        , badRequest :: { body :: ErrorMessage }
        )
  }
