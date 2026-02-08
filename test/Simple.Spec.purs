module Simple.Spec where

import Prelude

import Effect.Class.Console (log)
import Yoga.Fetch.Om.Simple (FetchError, FetchResponse, get, getWithHeaders, post, post_, put, put_, patch, patch_, delete, delete_)
import Yoga.Om (Om, handleErrors)

type User = { id :: Int, name :: String, email :: String }
type CreateUser = { name :: String, email :: String }

exampleGet :: forall ctx err. Om ctx (fetchError :: FetchError | err) User
exampleGet = get @User {} "https://api.example.com/users/42"

exampleGetWithAuth :: forall ctx err. Om ctx (fetchError :: FetchError | err) User
exampleGetWithAuth = get @User { authorization: "Bearer abc123" } "https://api.example.com/users/42"

exampleGetPlainText :: forall ctx err. Om ctx (fetchError :: FetchError | err) String
exampleGetPlainText = get @String {} "https://api.example.com/health"

exampleGetWithHeaders :: forall ctx err. Om ctx (fetchError :: FetchError | err) (FetchResponse User)
exampleGetWithHeaders = do
  { headers, body: user } <- getWithHeaders @User {} "https://api.example.com/users/42"
  log $ "Got user: " <> user.name
  pure { headers, body: user }

examplePost :: forall ctx err. Om ctx (fetchError :: FetchError | err) User
examplePost = post @User {} "https://api.example.com/users" { name: "Alice", email: "alice@example.com" }

examplePostFireAndForget :: forall ctx err. Om ctx (fetchError :: FetchError | err) Unit
examplePostFireAndForget = post_ {} "https://api.example.com/users" { name: "Alice", email: "alice@example.com" }

examplePut :: forall ctx err. Om ctx (fetchError :: FetchError | err) User
examplePut = put @User {} "https://api.example.com/users/42" { name: "Bob", email: "bob@example.com" }

examplePut_ :: forall ctx err. Om ctx (fetchError :: FetchError | err) Unit
examplePut_ = put_ {} "https://api.example.com/users/42" { name: "Bob", email: "bob@example.com" }

examplePatch :: forall ctx err. Om ctx (fetchError :: FetchError | err) User
examplePatch = patch @User {} "https://api.example.com/users/42" { name: "Bob" }

examplePatch_ :: forall ctx err. Om ctx (fetchError :: FetchError | err) Unit
examplePatch_ = patch_ {} "https://api.example.com/users/42" { name: "Bob" }

exampleDelete :: forall ctx err. Om ctx (fetchError :: FetchError | err) User
exampleDelete = delete @User {} "https://api.example.com/users/42"

exampleDelete_ :: forall ctx err. Om ctx (fetchError :: FetchError | err) Unit
exampleDelete_ = delete_ {} "https://api.example.com/users/42"

exampleHandleError :: forall ctx. Om ctx () Unit
exampleHandleError = do
  user <- get @User {} "https://api.example.com/users/42"
    # handleErrors { fetchError: \_ -> pure { id: 0, name: "unknown", email: "" } }
  log $ "Got user: " <> user.name

exampleComposition :: forall ctx. Om ctx () Unit
exampleComposition = do
  user <- post @User {} "https://api.example.com/users" { name: "Alice", email: "alice@example.com" }
    # handleErrors
        { fetchError: \e -> do
            log $ "Failed with status " <> show e.status <> ": " <> e.body
            pure { id: 0, name: "", email: "" }
        }
  _ <- put @User {} ("https://api.example.com/users/" <> show user.id) { name: "Alice Updated", email: user.email }
    # handleErrors { fetchError: \_ -> pure user }
  delete_ {} ("https://api.example.com/users/" <> show user.id)
    # handleErrors { fetchError: \_ -> pure unit }
