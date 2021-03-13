module Routing.Router where

import Prelude
import Control.Monad.Free.Trans (liftFreeT, runFreeT)
import Data.Foldable (class Foldable)
import Data.Lens (Lens', Prism', is, lens, prism')
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (error, killFiber, launchAff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Routing.Router.Types (Command(..), Driver(..), Resolved, Router(..), Transition(..), Transitioning)

makeRouter ::
  forall f i o.
  Foldable f =>
  Driver i o ->
  (Transition o -> Effect Unit) ->
  (o -> Router i o Transitioning Resolved Unit) ->
  Effect
    { initialize :: Effect (Effect Unit)
    , navigate :: i -> Effect Unit
    , redirect :: i -> Effect Unit
    }
makeRouter (Driver driver) onTransition onRoute = do
  fiberRef <- Ref.new (pure unit)
  previousRouteRef <- Ref.new Nothing
  let
    runRouter route = do
      -- kill the previous routing logic
      oldFiber <- Ref.read fiberRef
      launchAff_ (killFiber (error "Transition cancelled") oldFiber)
      -- run the supplied `onTransition` effect with the previous route
      previousRoute <- Ref.read previousRouteRef
      onTransition (Transitioning previousRoute route)
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
                  case cmd of
                    Redirect route' -> liftEffect do driver.redirect route'
                    Override route' -> finalise route'
                    Continue -> finalise route
                  mempty
      Ref.write fiber fiberRef
  pure { initialize: driver.initialize runRouter, navigate: driver.navigate, redirect: driver.redirect }

redirect :: forall i o. i -> Router i o Transitioning Resolved Unit
redirect route = Router (liftFreeT (Redirect route))

override :: forall i o. o -> Router i o Transitioning Resolved Unit
override route = Router (liftFreeT (Override route))

continue :: forall i o. Router i o Transitioning Resolved Unit
continue = Router (liftFreeT Continue)

_Transition :: forall route. Lens' (Transition route) route
_Transition = lens getter setter
  where
  getter = case _ of
    Transitioning _ route -> route
    Resolved _ route -> route

  setter = case _ of
    Transitioning route _ -> Transitioning route
    Resolved route _ -> Resolved route

_Transitioning :: forall route. Prism' (Transition route) route
_Transitioning =
  prism' (Transitioning Nothing) case _ of
    Transitioning _ route -> Just route
    _ -> Nothing

_Resolved :: forall route. Prism' (Transition route) route
_Resolved =
  prism' (Resolved Nothing) case _ of
    Resolved _ route -> Just route
    _ -> Nothing

isTransitioning :: forall route. Transition route -> Boolean
isTransitioning = is _Transitioning

isResolved :: forall route. Transition route -> Boolean
isResolved = is _Resolved
