module Wire.React.Router
  ( module Control
  , makeRouter
  ) where

import Prelude
import Control.Monad.Free.Trans (runFreeT)
import Data.Foldable (class Foldable, for_)
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
import Wire.React.Router.Control (Command(..), Resolved, Route(..), Router(..), Transitioning)
import Wire.React.Router.Control (Command, Resolved, Route(..), Router, Transitioning, _Resolved, _Route, _Transitioning, continue, isResolved, isTransitioning, override, redirect) as Control
import Wire.Signal (Signal)
import Wire.Signal as Signal

type Interface route
  = { signal :: Signal (Route route)
    , component :: JSX
    , navigate :: route -> Effect Unit
    , redirect :: route -> Effect Unit
    }

makeRouter ::
  forall f route.
  Foldable f =>
  PushStateInterface ->
  { fallback :: route
  , parse :: String -> f route
  , print :: route -> String
  , onRoute :: route -> Router route Transitioning Resolved Unit
  } ->
  Effect (Interface route)
makeRouter interface { fallback, parse, print, onRoute } =
  let
    onPushState k = PushState.matchesWith parse (\_ -> k) interface

    navigate route = interface.pushState (unsafeToForeign {}) (print route)

    redirect route = interface.replaceState (unsafeToForeign {}) (print route)
  in
    do
      { modify, signal } <- Signal.create (Transitioning Nothing fallback)
      do
        -- replace the user-supplied fallback route with the current route, if possible
        { path } <- interface.locationState
        for_ (parse path) \route -> modify \_ -> Transitioning Nothing route
      fiberRef <- Ref.new Nothing
      previousRouteRef <- Ref.new Nothing
      let
        runRouter route = do
          do
            -- if some previous long-running routing logic is still active, kill it
            oldFiber <- Ref.read fiberRef
            for_ oldFiber \fiber -> launchAff_ (killFiber (error "Transition cancelled") fiber)
          previousRoute <- Ref.read previousRouteRef
          -- set the route state to "transitioning" with the previous successful route
          modify \_ -> Transitioning previousRoute route
          let
            finalise r =
              liftEffect do
                Ref.write (Just r) previousRouteRef
                modify \_ -> Resolved previousRoute r
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
      component <-
        React.component "Wire.Router" \_ -> React.do
          React.useEffectOnce (onPushState runRouter)
          pure React.empty
      pure { signal, component: component unit, navigate, redirect }
