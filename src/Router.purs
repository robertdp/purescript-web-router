module Wire.React.Router
  ( module Control
  , makeRouter
  ) where

import Prelude
import Control.Monad.Free.Trans (runFreeT)
import Data.Foldable (class Foldable, traverse_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (error, killFiber, launchAff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Foreign (unsafeToForeign)
import React.Basic.Hooks (JSX)
import React.Basic.Hooks as React
import Routing.PushState (PushStateInterface)
import Routing.PushState as PushState
import Wire.React.Router.Control (Command(..), Resolved, Router(..), Transition(..), Transitioning)
import Wire.React.Router.Control (Command, Resolved, Router, Transition(..), Transitioning, _Resolved, _Transition, _Transitioning, continue, isResolved, isTransitioning, override, redirect) as Control

makeRouter ::
  forall f route.
  Foldable f =>
  PushStateInterface ->
  { parse :: String -> f route
  , print :: route -> String
  , onRoute :: route -> Router route Transitioning Resolved Unit
  , onTransition :: Transition route -> Effect Unit
  } ->
  Effect
    { component :: JSX
    , navigate :: route -> Effect Unit
    , redirect :: route -> Effect Unit
    }
makeRouter interface { parse, print, onRoute, onTransition } =
  let
    onPushState k = PushState.matchesWith parse (\_ -> k) interface

    navigate route = interface.pushState (unsafeToForeign {}) (print route)

    redirect route = interface.replaceState (unsafeToForeign {}) (print route)
  in
    do
      fiberRef <- Ref.new Nothing
      previousRouteRef <- Ref.new Nothing
      let
        runRouter route = do
          -- if some previous long-running routing logic is still active, kill it
          Ref.read fiberRef >>= traverse_ (launchAff_ <<< killFiber (error "Transition cancelled"))
          previousRoute <- Ref.read previousRouteRef
          -- set the route state to "transitioning" with the previous successful route
          onTransition $ Transitioning previousRoute route
          let
            finalise r =
              liftEffect do
                Ref.write (Just r) previousRouteRef
                onTransition $ Resolved previousRoute r
          fiber <-
            launchAff case onRoute route of
              Router router ->
                router
                  # runFreeT \cmd -> do
                      liftEffect do Ref.write Nothing fiberRef
                      case cmd of
                        Redirect route' -> liftEffect do redirect route'
                        Override route' -> finalise route'
                        Continue -> finalise route
                      mempty
          Ref.write (Just fiber) fiberRef
      -- run on the current path
      { path } <- interface.locationState
      traverse_ runRouter (parse path)
      component <-
        React.component "Wire.Router" \_ -> React.do
          React.useEffectOnce (onPushState runRouter)
          pure React.empty
      pure { component: component unit, navigate, redirect }
