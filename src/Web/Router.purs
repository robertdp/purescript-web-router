module Routing.Router where

import Prelude
import Control.Monad.Free.Trans (liftFreeT, runFreeT)
import Data.Lens (Lens', Prism', is, lens, prism')
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (error, killFiber, launchAff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Web.Router.Types (Command(..), Driver(..), Event(..), Resolved, Router, Transition(..), Transitioning, RouterSpec)

makeRouter ::
  forall i o.
  RouterSpec i o ->
  Effect (Router i)
makeRouter { driver: Driver driver, onEvent, onTransition } = do
  fiberRef <- Ref.new (pure unit)
  previousRouteRef <- Ref.new Nothing
  let
    runRouter route = do
      -- kill the previous routing logic
      oldFiber <- Ref.read fiberRef
      launchAff_ (killFiber (error "Transition cancelled") oldFiber)
      -- run the supplied `onTransition` effect with the previous route
      previousRoute <- Ref.read previousRouteRef
      onEvent (Transitioning previousRoute route)
      let
        finalise r =
          liftEffect do
            Ref.write (Just r) previousRouteRef
            onEvent $ Resolved previousRoute r
      fiber <-
        launchAff case onTransition previousRoute route of
          Transition router ->
            router
              # runFreeT \cmd -> do
                  case cmd of
                    Redirect route' -> liftEffect do driver.redirect route'
                    Override route' -> finalise route'
                    Continue -> finalise route
                  mempty
      Ref.write fiber fiberRef
  pure { initialize: driver.initialize runRouter, navigate: driver.navigate, redirect: driver.redirect }

redirect :: forall i o. i -> Transition i o Transitioning Resolved Unit
redirect route = Transition (liftFreeT (Redirect route))

override :: forall i o. o -> Transition i o Transitioning Resolved Unit
override route = Transition (liftFreeT (Override route))

continue :: forall i o. Transition i o Transitioning Resolved Unit
continue = Transition (liftFreeT Continue)

_Event :: forall route. Lens' (Event route) route
_Event = lens getter setter
  where
  getter = case _ of
    Transitioning _ route -> route
    Resolved _ route -> route

  setter = case _ of
    Transitioning route _ -> Transitioning route
    Resolved route _ -> Resolved route

_Transitioning :: forall route. Prism' (Event route) route
_Transitioning =
  prism' (Transitioning Nothing) case _ of
    Transitioning _ route -> Just route
    _ -> Nothing

_Resolved :: forall route. Prism' (Event route) route
_Resolved =
  prism' (Resolved Nothing) case _ of
    Resolved _ route -> Just route
    _ -> Nothing

isTransitioning :: forall route. Event route -> Boolean
isTransitioning = is _Transitioning

isResolved :: forall route. Event route -> Boolean
isResolved = is _Resolved
