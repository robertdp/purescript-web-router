{-| This example also requires the following dependencies to be installed:
    - react-basic-dom
    - routing-duplex
    - wire-react
-}
module Example.RoutingDuplex where

import Prelude hiding ((/))
import Data.Generic.Rep (class Generic)
import Data.Lens as Lens
import Effect (Effect)
import React.Basic.DOM as R
import React.Basic.Events (handler_)
import React.Basic.Hooks (JSX)
import React.Basic.Hooks as React
import Routing.Duplex (RouteDuplex', default, end, parse, print, root)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))
import Routing.PushState as PushState
import Wire.React as Wire
import Wire.React.Router (_Route)
import Wire.React.Router as Router

data Route
  = Home
  | About
  | NotFound

derive instance genericRoute :: Generic Route _

routes :: RouteDuplex' Route
routes =
  default NotFound
    $ root
    $ sum
        { "Home": end noArgs
        , "About": "about" / end noArgs
        , "NotFound": "_" / "not-found" / end noArgs
        }

makeApp :: Effect (Unit -> JSX)
makeApp = do
  interface <- PushState.makeInterface
  router <-
    Router.makeRouter
      { interface
      , fallback:
          -- used as the initial route if the parser fails
          NotFound
      , parse: parse routes
      , print: print routes
      , onRoute:
          -- this skips any async routing logic by accepting the parsed route immediately
          \route -> Router.continue
      }
  React.component "App" \props -> React.do
    route <-
      -- subscribe to the signal containing the current route
      Wire.useSignal router.signal
    pure
      $ React.fragment
          [ -- the router subscriber to pushstate events when this component is mounted, and unsubscribes when unmounted
            router.component
          , R.h1_
              [ R.text case Lens.view _Route route of
                  Home -> "Home"
                  About -> "About"
                  NotFound -> "Not Found"
              ]
          , R.div_
              [ R.text "Menu: "
              , R.button
                  { onClick: handler_ $ router.navigate Home
                  , children: [ R.text "Go to Home page" ]
                  }
              , R.button
                  { onClick: handler_ $ router.navigate About
                  , children: [ R.text "Go to About page" ]
                  }
              ]
          ]
