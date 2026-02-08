# purescript-yoga-fetch-om

Derive type-safe fetch clients from `purescript-yoga-http-api` route definitions.

## Installation

```bash
spago install yoga-fetch-om
```

## Progressive Examples

### Example 1: Simple GET Request

The simplest possible client - just fetch a user by ID:

```purescript
import Yoga.Fetch.Om (GET, Route, Path, type (/), type (:), deriveClient)

type SimpleAPI =
  { getUser ::
      Route GET
        (Path ("users" / "id" : Int))
        {}
        ( ok :: { body :: User } )
  }

api = deriveClient "https://api.example.com" (Proxy :: _ SimpleAPI)

main = do
  user <- api.getUser { id: 42 }
  log user.name  -- Type-safe! Compiler knows user is a User
```

**Key concepts:**
- `Path ("users" / "id" : Int)` defines URL structure with path parameter
- `{ id: 42 }` becomes `/users/42`
- Response is automatically parsed to `User`

### Example 2: Handling Multiple Response Types

Real APIs return different responses for success vs errors:

```purescript
type UserAPI =
  { getUser ::
      Route GET
        (Path ("users" / "id" : Int))
        {}
        ( ok :: { body :: User }
        , notFound :: { body :: ErrorMessage }
        )
  }

api = deriveClient "https://api.example.com" (Proxy :: _ UserAPI)

main = do
  user <- api.getUser { id: 42 }
    # handleErrors
        { notFound: \err -> do
            log $ "User not found: " <> err.error
            pure defaultUser
        }
  log user.name
```

**Key concepts:**
- Variant response types: `(ok :: ..., notFound :: ...)`
- `handleErrors` to convert error variants into values
- Type-safe error handling - can't forget a case!

### Example 3: Query Parameters

Add pagination with query strings:

```purescript
type UserAPI =
  { listUsers ::
      Route GET
        (Path "users" :? { limit :: Int, offset :: Int })
        {}
        ( ok :: { body :: Array User } )
  }

api = deriveClient "https://api.example.com" (Proxy :: _ UserAPI)

main = do
  users <- api.listUsers { limit: 10, offset: 20 }
  -- Calls /users?limit=10&offset=20
  log $ "Found " <> show (length users) <> " users"
```

**Key concepts:**
- `:?` operator adds query parameters
- `{ limit: 10, offset: 20 }` becomes `?limit=10&offset=20`
- Optional parameters use `Maybe` types

### Example 4: POST with JSON Body

Creating resources with request bodies:

```purescript
type UserAPI =
  { createUser ::
      Route POST
        (Path "users")
        { body :: JSON CreateUserRequest }
        ( created :: { body :: User }
        , badRequest :: { body :: ErrorMessage }
        )
  }

api = deriveClient "https://api.example.com" (Proxy :: _ UserAPI)

main = do
  user <- api.createUser
    { name: "Alice"
    , email: "alice@example.com"
    }
    # handleErrors
        { badRequest: \err -> do
            log $ "Validation error: " <> err.error
            throw err
        }
  log $ "Created user with ID: " <> show user.id
```

**Key concepts:**
- `{ body :: JSON CreateUserRequest }` defines request body
- Automatic JSON serialization with `yoga-json`
- Multiple response variants for different statuses

### Example 5: PUT/PATCH with Path Parameters + Body

Updating resources combines path params and body:

```purescript
type UserAPI =
  { updateUser ::
      Route PUT
        (Path ("users" / "id" : Int))
        { body :: JSON UpdateUserRequest }
        ( ok :: { body :: User }
        , notFound :: { body :: ErrorMessage }
        , badRequest :: { body :: ErrorMessage }
        )
  }

api = deriveClient "https://api.example.com" (Proxy :: _ UserAPI)

main = do
  user <- api.updateUser { id: 42 }
    { name: "Alice Updated"
    , email: "alice.new@example.com"
    }
    # handleErrors
        { notFound: \_ -> do
            log "User doesn't exist"
            throw userNotFound
        , badRequest: \err -> do
            log $ "Invalid update: " <> err.error
            throw validationError
        }
  log $ "Updated: " <> user.name
```

**Key concepts:**
- Path params (`{ id: 42 }`) and body are separate parameters
- Multiple error variants can be handled independently
- Exhaustive pattern matching ensures all cases handled

### Example 6: Complete CRUD API

Putting it all together - a full CRUD API:

```purescript
type UserAPI =
  { getUser ::
      Route GET (Path ("users" / "id" : Int)) {}
        ( ok :: { body :: User }
        , notFound :: { body :: ErrorMessage }
        )
  , listUsers ::
      Route GET (Path "users" :? { limit :: Int, offset :: Int }) {}
        ( ok :: { body :: Array User } )
  , createUser ::
      Route POST (Path "users") { body :: JSON CreateUserRequest }
        ( created :: { body :: User }
        , badRequest :: { body :: ErrorMessage }
        )
  , updateUser ::
      Route PUT (Path ("users" / "id" : Int)) { body :: JSON UpdateUserRequest }
        ( ok :: { body :: User }
        , notFound :: { body :: ErrorMessage }
        , badRequest :: { body :: ErrorMessage }
        )
  , deleteUser ::
      Route DELETE (Path ("users" / "id" : Int)) {}
        ( noContent :: { body :: {} }
        , notFound :: { body :: ErrorMessage }
        )
  }

api = deriveClient "https://api.example.com" (Proxy :: _ UserAPI)

-- Use it anywhere in your app!
userWorkflow = do
  -- Create
  user <- api.createUser { name: "Alice", email: "alice@example.com" }
  
  -- Read
  user <- api.getUser { id: user.id }
  
  -- Update
  user <- api.updateUser { id: user.id } { name: "Alice Updated", email: user.email }
  
  -- Delete
  _ <- api.deleteUser { id: user.id }
  
  log "Workflow complete!"
```

**Key concepts:**
- Single source of truth for your API
- All CRUD operations type-safe and derived automatically
- Share route definitions between client and server (with `yoga-fastify-om`)

### Example 7: Integration with Om Monad

Using with `yoga-om` for effects and dependency injection:

```purescript
type AppContext =
  { api :: UserAPI
  , userId :: Int
  }

type AppErrors =
  ( userNotFound :: Int
  , unauthorized :: String
  )

getUserProfile :: Om AppContext AppErrors User
getUserProfile = do
  { api, userId } <- ask
  api.getUser { id: userId }
    # handleErrors
        { notFound: \_ -> throw { userNotFound: userId }
        }

updateProfile :: UpdateUserRequest -> Om AppContext AppErrors User
updateProfile updates = do
  { api, userId } <- ask
  api.updateUser { id: userId } updates
    # handleErrors
        { notFound: \_ -> throw { userNotFound: userId }
        , badRequest: \err -> throw { unauthorized: err.error }
        }
```

**Key concepts:**
- API client lives in context (dependency injection)
- Error handling with typed variants
- Composable with other Om-based effects

## Complete Example

See [`test/Complete.Example.purs`](test/Complete.Example.purs) for a complete working example with all patterns.

## Features

### ✅ Type-Safe Everything

- **Path parameters**: Compiler ensures `/users/:id` gets an `id`
- **Query parameters**: Type-safe query strings
- **Request bodies**: Automatic JSON serialization
- **Response bodies**: Automatic JSON parsing
- **Error handling**: Exhaustive pattern matching on response variants

### ✅ Single Source of Truth

Define your API once, use it everywhere:

```purescript
-- Server (yoga-fastify-om)
server = buildServer apiRoutes handlers

-- Client (yoga-fetch-om)
client = deriveClient baseUrl (Proxy :: _ apiRoutes)
```

Changes to routes automatically update both client and server!

### ✅ Automatic Derivation

No manual client code needed:
- ✅ URL building with path parameter substitution
- ✅ Query string construction
- ✅ JSON request serialization
- ✅ JSON response parsing
- ✅ Status code → variant mapping
- ✅ CORS and credentials handling

### ✅ Integration with yoga-om

Works seamlessly with the Om monad for effects and dependency injection.

## How It Works

The library uses PureScript's type system to:

1. **Extract parameters** from route definitions at compile time
   - Path params from segments: `"users" / "id" : Int`
   - Query params from `:?` operator: `:? { limit :: Int }`
   - Body params from Request type: `{ body :: JSON User }`

2. **Build URLs** by:
   - Getting the path pattern: `/users/:id`
   - Substituting path params: `/users/42`
   - Appending query params: `?limit=10&offset=20`

3. **Make requests** using:
   - `js-fetch` for the fetch API
   - `js-promise-aff` to convert Promises → Aff
   - `yoga-json` for JSON serialization

4. **Parse responses** by:
   - Mapping status codes to variant labels (200 → "ok", 404 → "notFound")
   - Parsing JSON response bodies
   - Constructing typed Response values

All of this happens automatically based on your route types!

## API Reference

### `deriveClient`

```purescript
deriveClient :: String -> Proxy (Record routes) -> Record clients
```

Derives a record of client functions from a record of routes.

**Parameters:**
- `baseUrl` - The base URL of the API (e.g., `"https://api.example.com"`)
- `routes` - Proxy of your API route definitions

**Returns:** A record where each route becomes a function taking params and returning `Om context errors result`

## Roadmap

### Current Version (0.1.0)
- ✅ Core client derivation
- ✅ All HTTP methods (GET, POST, PUT, PATCH, DELETE)
- ✅ Path, query, and body parameters
- ✅ JSON encoding/decoding
- ✅ Variant response handling
- ✅ CORS support
- ✅ Om monad integration

### Future
- ⏳ Request/response headers support
- ⏳ Authentication (Bearer tokens, API keys)
- ⏳ Request/response interceptors
- ⏳ Retry logic with exponential backoff
- ⏳ Timeout configuration
- ⏳ FormData and file upload support
- ⏳ Progress events
- ⏳ Request cancellation

## Contributing

Contributions welcome! This library follows the patterns established by the yoga ecosystem:
- Type-level programming for automatic derivation
- Integration with yoga-json for serialization
- Consistent API design with yoga-fastify-om

## License

MIT

## Related Projects

- [purescript-yoga-http-api](https://github.com/rowtype-yoga/purescript-yoga-http-api) - HTTP API type definitions
- [purescript-yoga-fastify-om](https://github.com/rowtype-yoga/purescript-yoga-fastify-om) - Server-side counterpart
- [purescript-yoga-json](https://github.com/rowtype-yoga/purescript-yoga-json) - JSON serialization
- [purescript-yoga-om](https://github.com/rowtype-yoga/purescript-yoga-om) - Om monad for effects and DI
