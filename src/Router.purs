module React.Basic.Hooks.Router
  ( RouterContext
  , create
  , createContext
  , UseRouter
  , useTransition
  , useRoute
  , module Exports
  ) where

import Prelude
import Data.Either (Either(..))
import Data.Foldable (class Foldable, for_)
import Data.Lens (review, view)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Effect (Effect)
import Effect.Aff (error, killFiber, launchAff_, runAff)
import Effect.Console as Console
import Effect.Ref as Ref
import React.Basic (JSX, ReactContext)
import React.Basic.Hooks (UseContext, Hook)
import React.Basic.Hooks as React
import React.Basic.Hooks.Router.Control (Command(..), Completed, Pending, Router, Transition(..), _Completed, _Transition, runRouter)
import React.Basic.Hooks.Router.Signal (Signal, UseSignal)
import React.Basic.Hooks.Router.Signal as Signal
import Routing.PushState (PushStateInterface)
import Routing.PushState as Routing
import React.Basic.Hooks.Router.Control (Completed, Pending, Router, Transition(..), _Completed, _Pending, _Transition, continue, isCompleted, isPending, override, redirect) as Exports

newtype RouterContext route
  = RouterContext (ReactContext (Signal (Transition route)))

create ::
  forall f route.
  Foldable f =>
  { context :: RouterContext route
  , interface :: PushStateInterface
  , initial :: route
  , parser :: String -> f route
  , navigate :: route -> Router route Pending Completed Unit
  , redirect :: route -> Effect Unit
  } ->
  Effect (Array JSX -> JSX)
create { context: RouterContext context, interface, initial, parser, navigate, redirect } = do
  router <- mkRouter
  pure \content -> router { content }
  where
  mkRouter = do
    transition <- Signal.create $ review _Completed initial
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
                navigate route
                  # runRouter
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

createContext :: forall route. route -> Effect (RouterContext route)
createContext route = do
  signal <- Signal.create $ review _Completed route
  RouterContext <$> React.createContext signal

newtype UseRouter route hooks
  = UseRouter (UseSignal (Transition route) (UseContext (Signal (Transition route)) hooks))

derive instance newtypeUseRouter :: Newtype (UseRouter a hooks) _

useTransition :: forall route. RouterContext route -> Hook (UseRouter route) (Transition route)
useTransition (RouterContext context) =
  React.coerceHook React.do
    signal <- React.useContext context
    Signal.useSignal signal

useRoute :: forall route. RouterContext route -> Hook (UseRouter route) route
useRoute context = view _Transition <$> useTransition context
