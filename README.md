# purescript-web-router

A router for browsers that supports asynchronous routing logic. Bring your own printing and parsing (check out [routing-duplex](https://github.com/natefaubion/purescript-routing-duplex)).

For a basic React example see [here](https://github.com/robertdp/purescript-web-router-example/tree/master/src).

## How to use

### 1. Install with Spago

`$ spago install web-router`

### 2. Define your routes

```purescript
data Route
  = Page Page
  | NotFound

data Page
  = Home
  | ProductList
  | ProductView ProductId
  | About
  | ContactUs

type ProductId = Int
```

### 3. Implement parsing and printing

This example uses [routing-duplex](https://github.com/natefaubion/purescript-routing-duplex).

<details>
<summary>Imports</summary>

```purescript
import Prelude hiding ((/))
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Routing.Duplex (RouteDuplex', default, end, int, parse, print, root, segment)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))
import Routing.Duplex.Parser (RouteError)
```

</details>

```purescript
derive instance Generic Route _
derive instance Generic Page _

productId :: RouteDuplex' ProductId
productId = int segment

routes :: RouteDuplex' Route
routes =
  default NotFound $
    sum
      { "Page": pages
      , "NotFound": "404" / noArgs
      }

pages :: RouteDuplex' Page
pages =
  root $ end $
    sum
      { "Home": noArgs
      , "ProductList": "products" / noArgs
      , "ProductView": "products" / productId
      , "About": "about" / noArgs
      , "ContactUs": "about" / noArgs
      }

-- | This is the route parser we need to pass to the driver.
-- | It can produce any route which allows the parser to return a value of `NotFound` instead of failing.
parseRoute :: forall  String -> Either RouteError Route
parseRoute = parse routes

-- | This is the route printer we need to pass to the driver.
-- | It can only print paths to valid pages, which means a path can't be produced for the `NotFound` route.
-- | With this approach routes can be seperated based on whether they should be a navigation target and have a URL.
-- | Note: assymetry is not required, and a symmetrical printer works as well.
printRoute :: Page -> String
printRoute = print pages
```

### 4. Define how your application reacts to navigation and routing events

<details>
<summary>Imports</summary>

```purescript
import Web.Router as Router
```

</details>

```purescript
onNavigation :: Maybe Route -> Route -> Router.RouterM Route Page Router.Routing Router.Resolved Unit
onNavigation previousRoute requestedRoute =
  case requestedRoute of
    NotFound ->
      case previousRoute of
        Just (Page page) -> Router.do
          liftEffect showBrokenNavigationMessage
          Router.redirect page -- redirect back to the previous page and show a message
        _ ->
          Router.continue -- no previous page, so just show the "not found" page
    _ -> Router.do
      access <- liftAff fetchUserAccess
      if userHasAccess requestedRoute access then
        Router.continue -- they have access, so resolve with the requested page
      else
        Router.override NotFound -- no access, so pretend the page doesn't exist


onEvent :: Router.RoutingEvent Route -> Effect Unit
onEvent newEvent =
  case newEvent of
    Router.Routing previousRoute requestedRoute ->
      showNavigationSpinner
    Router.Resolved previousRoute newRoute ->
      hideNavigationSpinner
      setCurrentRoute newRoute
```

### 5. Connect up the driver and router

<details>
<summary>Imports</summary>

```purescript
import Web.Router as Router
import Web.Router.PushState as PushState
```

</details>

```purescript
mkRouter :: Effect (Router.Router Route Page)
mkRouter = do
  driver <- PushState.mkInterface parseRoute printRoute
  router <- Router.mkInterface onNavigation onEvent driver
  pure router
```

Both pushstate and hash drivers are included, or a custom driver can be implemented. An example of a custom driver could be one that synchronises some navigation state over sockets, for an experience where one user's behaviour could be broadcast to multiple users to follow along.
