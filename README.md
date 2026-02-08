# purescript-yoga-fetch-om

Derive type-safe fetch clients from `purescript-yoga-http-api` route definitions.

## Concept

Similar to how `purescript-yoga-fastify-om` derives servers from HTTP API definitions, this library derives **clients** from the same definitions using the Fetch API.

## Installation

```bash
spago install yoga-fetch-om
```

## Quick Start

### 1. Define your API using yoga-http-api

```purescript
import Yoga.Fetch.Om

type UserAPI =
  { getUser ::
      Route GET
        (Path ("users" / "id" : Int))
        (Request {})
        ( ok :: { body :: User }
        , notFound :: { body :: ErrorMessage }
        )
  , createUser ::
      Route POST
        (Path "users")
        (Request { body :: JSON CreateUserRequest })
        ( created :: { body :: User }
        , badRequest :: { body :: ErrorMessage }
        )
  }
```

### 2. Derive a type-safe client

```purescript
api = deriveAPI "https://api.example.com" (Proxy :: _ UserAPI)
```

### 3. Use it!

```purescript
main = do
  -- Compiler ensures you pass the right parameters
  result <- api.getUser { id: 42 }

  -- Pattern match on all possible responses
  result # Variant.match
    { ok: \(Response { body: user }) ->
        log $ "User: " <> user.name
    , notFound: \_ ->
        log "User not found"
    }
```

## Features

### ✅ Type-Safe Parameters

The compiler ensures you provide exactly the right parameters:

```purescript
-- Path parameters
api.getUser { id: 42 }

-- Query parameters
api.listUsers { limit: 10, offset: 20 }

-- Request body
api.createUser { body: { name: "Alice", email: "alice@example.com" } }

-- Mixed parameters
api.updateUser { id: 42, body: { name: "Alice Updated" } }
```

### ✅ Exhaustive Response Handling

Variant types ensure you handle all possible responses:

```purescript
result <- api.createUser { body: newUser }

result # Variant.match
  { created: \(Response { body }) -> log $ "Created: " <> show body.id
  , badRequest: \(Response { body }) -> log $ "Error: " <> body.error
  -- Forgot a case? Compile error!
  }
```

### ✅ Single Source of Truth

Define your API once, use it everywhere:

```purescript
-- Server (yoga-fastify-om)
server = buildServer apiRoutes handlers

-- Client (yoga-fetch-om)
client = deriveAPI baseUrl (Proxy :: _ apiRoutes)
```

Changes to routes automatically update both client and server!

### ✅ Automatic Everything

No manual client code needed:
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
  result <- liftAff $ api.getUser { id: userId }
  case result of
    ok -> pure result.body
    notFound -> throw { userNotFound: userId }
```

## Complete Example

See [`test/Complete.Example.purs`](test/Complete.Example.purs) for a full working example with:
- GET requests with path parameters
- GET requests with query parameters
- POST/PUT requests with JSON bodies
- DELETE requests
- Complete error handling

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

### `deriveAPI`

```purescript
deriveAPI :: String -> Proxy (Record routes) -> Record clients
```

Derives a record of client functions from a record of routes.

**Parameters:**
- `baseUrl` - The base URL of the API (e.g., `"https://api.example.com"`)
- `routes` - Proxy of your API route definitions

**Returns:** A record where each route becomes a function taking params and returning `Aff (Variant response)`

### `deriveClientFn`

```purescript
deriveClientFn :: String -> Proxy (Route method segments request response) -> ClientFn params response
```

Derives a single client function from a route. Usually you'll use `deriveAPI` instead, but this is useful for individual routes.

## Roadmap

### Current Version (0.1.0)
- ✅ Core client derivation
- ✅ All HTTP methods (GET, POST, PUT, PATCH, DELETE)
- ✅ Path, query, and body parameters
- ✅ JSON encoding/decoding
- ✅ Variant response handling
- ✅ CORS support

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
