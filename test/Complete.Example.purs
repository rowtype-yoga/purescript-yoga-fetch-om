module Complete.Example where

import Prelude

import Data.Array (length)
import Justifill (justifill)
import Effect.Class.Console (log)
import Yoga.Fetch.Om (GET, POST, PUT, DELETE, Route, JSON, type (/), type (:), type (:?), client)
import Yoga.HTTP.API.Route (BearerToken(..))
import Yoga.Om (Om, handleErrors)

type User =
  { id :: Int
  , name :: String
  , email :: String
  }

type CreateUserRequest =
  { name :: String
  , email :: String
  }

type UpdateUserRequest =
  { name :: String
  , email :: String
  }

type ErrorMessage =
  { error :: String
  }

type UserAPI =
  { getUser ::
      Route GET ("users" / "id" : Int)
        {}
        ( ok :: { body :: User }
        , notFound :: { body :: ErrorMessage }
        )
  , listUsers ::
      Route GET ("users" :? { limit :: Int, offset :: Int })
        {}
        ( ok :: { body :: Array User }
        )
  , createUser ::
      Route POST "users"
        { body :: JSON CreateUserRequest }
        ( created :: { body :: User }
        , badRequest :: { body :: ErrorMessage }
        )
  , updateUser ::
      Route PUT ("users" / "id" : Int)
        { body :: JSON UpdateUserRequest }
        ( ok :: { body :: User }
        , notFound :: { body :: ErrorMessage }
        , badRequest :: { body :: ErrorMessage }
        )
  , deleteUser ::
      Route DELETE ("users" / "id" : Int)
        {}
        ( noContent :: {}
        , notFound :: { body :: ErrorMessage }
        )
  , createUserAuth ::
      Route POST "users"
        { headers :: Record (authorization :: BearerToken), body :: JSON CreateUserRequest }
        ( created :: { body :: User }
        , badRequest :: { body :: ErrorMessage }
        )
  }

api = client @UserAPI "https://api.example.com"

exampleGetUser :: Om {} (notFound :: ErrorMessage) Unit
exampleGetUser = do
  user <- api.getUser { id: 42 }
  log $ "Found user: " <> user.name <> " (" <> user.email <> ")"

exampleGetUserHandled :: Om {} () Unit
exampleGetUserHandled = do
  user <- api.getUser { id: 42 }
    # handleErrors
        { notFound: \err -> ado
            log $ "User not found: " <> err.error
            in { id: 0, name: "unknown", email: "" }
        }
  log $ "Found user: " <> user.name

exampleListUsers :: Om {} () Unit
exampleListUsers = do
  users <- api.listUsers (justifill { limit: 10, offset: 0 })
  log $ "Found " <> show (length users) <> " users"

exampleCreateUser :: Om {} (badRequest :: ErrorMessage) Unit
exampleCreateUser = do
  user <- api.createUser
    { name: "Alice"
    , email: "alice@example.com"
    }
  log $ "Created user with ID: " <> show user.id

exampleUpdateUser :: Om {} (notFound :: ErrorMessage, badRequest :: ErrorMessage) Unit
exampleUpdateUser = do
  user <- api.updateUser
    { id: 42 }
    { name: "Alice Updated"
    , email: "alice.new@example.com"
    }
  log $ "Updated user: " <> user.name

exampleDeleteUser :: Om {} (notFound :: ErrorMessage) Unit
exampleDeleteUser = do
  _ <- api.deleteUser { id: 42 }
  log "User deleted successfully"

exampleCreateUserAuth :: Om {} (badRequest :: ErrorMessage) Unit
exampleCreateUserAuth = do
  user <- api.createUserAuth { authorization: BearerToken "abc123" }
    { name: "Alice"
    , email: "alice@example.com"
    }
  log $ "Created user with ID: " <> show user.id
