module Wire.React.Router
  ( module Control
  , makeRouter
  ) where

import Prelude
import Control.Monad.Free.Trans (runFreeT)
import Data.Foldable (class Foldable, for_, traverse_)
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

makeRouter ::
  forall route f.
  Foldable f =>
  { interface :: PushStateInterface
  , default :: route
  , decode :: String -> f route
  , encode :: route -> String
  , onRouteChange :: route -> Router route Transitioning Resolved Unit
  } ->
  Effect
    { signal :: Signal (Route route)
    , router :: JSX
    , navigate :: route -> Effect Unit
    , redirect :: route -> Effect Unit
    }
makeRouter { interface, default, decode, encode, onRouteChange } =
  let
    onPushState k = PushState.matchesWith decode (\_ -> k) interface

    navigate route = interface.pushState (unsafeToForeign {}) (encode route)

    redirect route = interface.replaceState (unsafeToForeign {}) (encode route)
  in
    do
      { modify, signal } <- Signal.create (Transitioning Nothing default)
      -- replace the user-supplied default route with the current route, if possible
      interface.locationState >>= \{ path } -> for_ (decode path) \route -> modify \_ -> Transitioning Nothing route
      fiberRef <- Ref.new Nothing
      previousRouteRef <- Ref.new Nothing
      let
        runRouter route = do
          -- if some previous long-running routing logic is still active, kill it
          Ref.read fiberRef >>= traverse_ (launchAff_ <<< killFiber (error "Transition cancelled"))
          previousRoute <- Ref.read previousRouteRef
          modify \_ -> Transitioning previousRoute route
          let
            finalise r =
              liftEffect do
                Ref.write (Just r) previousRouteRef
                modify \_ -> Resolved previousRoute r
          fiber <-
            launchAff case onRouteChange route of
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
          React.useEffectOnce do onPushState runRouter
          pure React.empty
      pure { signal, router: component unit, navigate, redirect }
