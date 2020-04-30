module React.Basic.Hooks.Routing where

import Prelude
import Data.Either (Either(..))
import Data.Foldable (class Foldable, for_)
import Data.Maybe (Maybe(..))
import Data.Nullable (null)
import Effect (Effect)
import Effect.Aff (error, killFiber, launchAff_, runAff)
import Effect.Console as Console
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import React.Basic (JSX, ReactContext)
import React.Basic.Hooks as React
import React.Basic.Hooks.Routing.Control (Command(..), Completed, Pending, Routing, Transition(..), runRouting)
import React.Basic.Hooks.Routing.Signal (Signal)
import React.Basic.Hooks.Routing.Signal as Signal
import Routing.PushState (PushStateInterface)
import Routing.PushState as Routing
import Unsafe.Coerce (unsafeCoerce)

context :: forall route. ReactContext (Signal route)
context = unsafePerformEffect do React.createContext do unsafePerformEffect do Signal.create (unsafeCoerce null)

create ::
  forall f route.
  Foldable f =>
  { interface :: PushStateInterface
  , initial :: Transition route
  , parser :: String -> f route
  , onRoute :: route -> Routing route Pending Completed Unit
  , redirect :: route -> Effect Unit
  } ->
  Effect (Array JSX -> JSX)
create { interface, initial, parser, onRoute, redirect } = do
  router <- mkRouter
  pure \content -> router { content }
  where
  mkRouter = do
    transition <- Signal.create initial
    fiberRef <- Ref.new Nothing
    previousRouteRef <- Ref.new Nothing
    React.component "Router" \props -> React.do
      React.useEffect unit do
        interface
          # Routing.matchesWith parser \_ route -> do
              oldFiber <- Ref.read fiberRef
              for_ oldFiber $ launchAff_ <<< killFiber (error "Transition cancelled")
              previousRoute <- Ref.read previousRouteRef
              Signal.write transition (Pending previousRoute route)
              let
                writeOut r = do
                  Ref.write (Just r) previousRouteRef
                  Signal.write transition (Completed previousRoute r)
              fiber <-
                onRoute route
                  # runRouting
                  # runAff \res -> do
                      Ref.write Nothing fiberRef
                      case res of
                        Left err -> do
                          Console.errorShow err
                        Right cmd -> case cmd of
                          Redirect route' -> redirect route'
                          Override route' -> writeOut route'
                          Continue -> writeOut route
              Ref.write (Just fiber) fiberRef
      pure $ React.provider context transition props.content
