# purescript-web-router

A basic router for browsers with support for asynchronous routing logic. Bring your own printing and parsing (check out [routing-duplex](https://github.com/natefaubion/purescript-routing-duplex)).

For a basic example see [here](https://github.com/robertdp/purescript-web-router-example/tree/master/src).

## How to use

### 1. Install with Spago

`$ spago install web-router` (coming soon)

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
<pre>
import Prelude hiding ((/))
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Routing.Duplex (RouteDuplex', default, int, parse, print, root, segment)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))
import Routing.Duplex.Parser (RouteError)
</pre>
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
  root $
    sum
      { "Home": noArgs
      , "ProductList": "products" / noArgs
      , "ProductView": "products" / productId
      , "About": "about" / noArgs
      , "ContactUs": "about" / noArgs
      }

-- | This is route parser we need to pass to the driver.
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

### Define how your application reacts to navigation and routing events

<details>
<summary>Imports</summary>
<pre>
import Web.Router as Router
</pre>
</details>

```purescript
onNavigation :: Maybe Route -> Route -> RouterM Route Page Routing Resolved Unit
onNavigation previousRoute requestedRoute = do
  Router.continue -- accept the navigation to the requested route

onEvent :: RoutingEvent Route -> Effect Unit
onEvent newEvent =
  case newEvent of
    Router.Routing oldRoute newRoute ->
      showNavigationSpinner
    Router.Resolved oldRoute newRoute ->
      hideNavigationSpinner
      setCurrentRoute newRoute
```

### Connect up the driver and router

<details>
<summary>Imports</summary>
<pre>
import Web.Router as Router
import Web.Router.Driver.PushState as PushState
</pre>
</details>

```purescript
mkRouter :: Effect (Router.Router Route Page)
mkRouter = do
  driver <- PushState.mkDriver parseRoute printRoute
  router <- Router.mkRouter onNavigation onEvent
  pure router
```

Both pushstate and hash drivers are included, or a custom driver can be implemented. An example of a custom driver could be one that synchronises some navigation state over sockets, allowing an experience where one users behaviour could be broadcast to multiple users to follow along.
