# purescript-yoga-fetch-om

Derive type-safe fetch clients from `purescript-yoga-http-api` route definitions.

## Installation

```bash
spago install yoga-fetch-om
```

## Quick Start

### Example 1: Simple GET and POST Requests

```purescript
import Yoga.Fetch.Om.Simple (get, post)

type User = { id :: Int, name :: String, email :: String }

main = do
  -- Simple GET request
  user <- get @User "https://api.example.com/users/42" {}
  log user.name
  
  -- Simple POST request with JSON body
  newUser <- post @User "https://api.example.com/users" {} { name: "Alice", email: "alice@example.com" }
  log $ "Created user with ID: " <> show newUser.id
```

### Example 2: Type-Safe API Client with Full CRUD

```purescript
type UserAPI =
  { getUser ::
      Route GET ("users" / "id" : Int) {}
        ( ok :: { body :: User }
        , notFound :: { body :: ErrorMessage }
        )
  , listUsers ::
      Route GET ("users" :? { limit :: Int, offset :: Int }) {}
        ( ok :: { body :: Array User } )
  , createUser ::
      Route POST "users" { body :: JSON CreateUserRequest }
        ( created :: { body :: User }
        , badRequest :: { body :: ErrorMessage }
        )
  , updateUser ::
      Route PUT ("users" / "id" : Int) { body :: JSON UpdateUserRequest }
        ( ok :: { body :: User }
        , notFound :: { body :: ErrorMessage }
        , badRequest :: { body :: ErrorMessage }
        )
  , deleteUser ::
      Route DELETE ("users" / "id" : Int) {}
        ( noContent :: { body :: {} }
        , notFound :: { body :: ErrorMessage }
        )
  }

api = client @UserAPI "https://api.example.com"

-- Create a user
user <- api.createUser { name: "Alice", email: "alice@example.com" }
  # handleErrors
      { badRequest: \err -> do
          log $ "Validation error: " <> err.error
          throw err
      }

-- Update with path param + body
updated <- api.updateUser { id: user.id } { name: "Alice Updated", email: user.email }
  # handleErrors
      { notFound: \_ -> throw userNotFound
      , badRequest: \err -> throw validationError
      }

-- Query with pagination
users <- api.listUsers { limit: 10, offset: 0 }
```

## More Examples

See the test files for complete, runnable examples with all imports:

- **[`test/Simple.Spec.purs`](test/Simple.Spec.purs)** - Basic GET/POST/PUT/PATCH/DELETE requests
- **[`test/Complete.Example.purs`](test/Complete.Example.purs)** - Full CRUD API with error handling
- **[`test/BuildUrl.Spec.purs`](test/BuildUrl.Spec.purs)** - Path parameters and query strings
- **[`test/Variant.Spec.purs`](test/Variant.Spec.purs)** - Response variant handling
- **[`test/SplitParams.Spec.purs`](test/SplitParams.Spec.purs)** - Parameter extraction patterns

## Features

### ✅ Type-Safe Everything

- **Path parameters**: `/users/:id` requires `{ id: Int }`
- **Query parameters**: Type-safe query strings with `?`
- **Request bodies**: Automatic JSON serialization
- **Response bodies**: Automatic JSON parsing
- **Error handling**: Exhaustive pattern matching on variants

### ✅ Single Source of Truth

Define your API once, use it everywhere:

```purescript
-- Server (yoga-fastify-om)
server = buildServer apiRoutes handlers

-- Client (yoga-fetch-om)
apiClient = client @apiRoutes baseUrl
```

Changes to routes automatically update both client and server!

### ✅ Automatic Derivation

No manual client code:
- ✅ URL building with path parameter substitution
- ✅ Query string construction
- ✅ JSON request serialization
- ✅ JSON response parsing
- ✅ Status code → variant mapping
- ✅ CORS and credentials handling

### ✅ Integration with yoga-om

Works seamlessly with the Om monad:

```purescript
getUserProfile :: Om AppContext AppErrors User
getUserProfile = do
  { api, userId } <- ask
  api.getUser { id: userId }
    # handleErrors { notFound: \_ -> throw { userNotFound: userId } }
```

## How It Works

The library uses PureScript's type system to:

1. **Extract parameters** from route definitions at compile time
   - Path params: `"users" / "id" : Int` → `{ id :: Int }`
   - Query params: `:? { limit :: Int }` → `{ limit :: Int }`
   - Body params: `{ body :: JSON User }` → request body

2. **Build URLs** automatically
   - Pattern: `/users/:id` + params: `{ id: 42 }` → `/users/42`
   - Query: `?limit=10&offset=20`

3. **Make requests** with `js-fetch` and `js-promise-aff`

4. **Parse responses** by mapping status codes to variant labels
   - `200` → `"ok"`, `404` → `"notFound"`, etc.

All automatically based on your route types!

## API Reference

### `client`

```purescript
client :: forall @routes. String -> Record clients
```

Derives a record of client functions from a record of routes using Visible Type Application.

**Parameters:**
- `@routes` - Your API route type (provided via VTA syntax)
- `baseUrl` - Base URL (e.g., `"https://api.example.com"`)

**Returns:** Record where each route becomes a function returning `Om context errors result`

**Example:**
```purescript
api = client @UserAPI "https://api.example.com"
```

## Features

- ✅ Core client derivation with VTA syntax
- ✅ All HTTP methods (GET, POST, PUT, PATCH, DELETE)
- ✅ Path, query, and body parameters
- ✅ Request/response headers
- ✅ JSON encoding/decoding
- ✅ FormData support
- ✅ Variant response handling
- ✅ Om monad integration

## License

MIT

## Related Projects

- [purescript-yoga-http-api](https://github.com/rowtype-yoga/purescript-yoga-http-api) - HTTP API type definitions
- [purescript-yoga-fastify-om](https://github.com/rowtype-yoga/purescript-yoga-fastify-om) - Server-side counterpart
- [purescript-yoga-json](https://github.com/rowtype-yoga/purescript-yoga-json) - JSON serialization
- [purescript-yoga-om](https://github.com/rowtype-yoga/purescript-yoga-om) - Om monad
