module Complete.Example where

import Prelude

import Data.Array (length)
import Data.Maybe (Maybe(..))
import Effect.Class.Console (log)
import Yoga.Fetch.Om (GET, POST, PUT, DELETE, Route, JSON, Path, type (/), type (:), type (:?), client)
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
  , updateUser ::
      Route PUT
        (Path ("users" / "id" : Int))
        { body :: JSON UpdateUserRequest }
        ( ok :: { body :: User }
        , notFound :: { body :: ErrorMessage }
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

api
  :: { createUser :: CreateUserRequest -> Om {} (badRequest :: ErrorMessage) User
     , createUserAuth :: { authorization :: String } -> CreateUserRequest -> Om {} (badRequest :: ErrorMessage) User
     , deleteUser :: { id :: Int } -> Om {} (notFound :: ErrorMessage) {}
     , getUser :: { id :: Int } -> Om {} (notFound :: ErrorMessage) User
     , listUsers :: { limit :: Maybe Int, offset :: Maybe Int } -> Om {} () (Array User)
     , updateUser :: { id :: Int } -> UpdateUserRequest -> Om {} (badRequest :: ErrorMessage, notFound :: ErrorMessage) User
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
        { notFound: \err -> do
            log $ "User not found: " <> err.error
            pure { id: 0, name: "unknown", email: "" }
        }
  log $ "Found user: " <> user.name

exampleListUsers :: Om {} () Unit
exampleListUsers = do
  users <- api.listUsers { limit: Just 10, offset: Just 0 }
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
  user <- api.updateUser { id: 42 }
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
  user <- api.createUserAuth { authorization: "Bearer abc123" }
    { name: "Alice"
    , email: "alice@example.com"
    }
  log $ "Created user with ID: " <> show user.id
