module Web.Router (module Exports, mkRouter) where

import Prelude
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Web.Router.Internal.Control (Command(..), Resolved, RouterM, Routing, runRouter)
import Web.Router.Internal.Control (Resolved, RouterM, Routing) as Exports
import Web.Router.Internal.Types (Driver(..), Router, RouterEvent(..))
import Web.Router.Internal.Types (Driver, Router, RouterEvent(..), _Resolved, _RouterEvent, _Routing, isResolved, isRouting) as Exports

mkRouter ::
  forall i o.
  (Maybe i -> i -> RouterM i o Routing Resolved Unit) ->
  (RouterEvent i -> Effect Unit) ->
  Driver i o ->
  Effect (Router o)
mkRouter onRouteStart onEvent (Driver driver) = do
  lastEventRef <- Ref.new Nothing
  let
    readPreviousRoute :: Effect (Maybe i)
    readPreviousRoute =
      Ref.read lastEventRef
        <#> case _ of
            Just (Resolved _ route) -> Just route
            Just (Routing (Just route) _) -> Just route
            _ -> Nothing

    handleEvent :: RouterEvent i -> Effect Unit
    handleEvent event = do
      Ref.write (Just event) lastEventRef
      onEvent event

    onCommand :: forall a. i -> Command i o a -> Aff Unit
    onCommand newRoute cmd =
      liftEffect do
        previousRoute <- readPreviousRoute
        case cmd of
          Continue -> handleEvent (Resolved previousRoute newRoute)
          Override route -> handleEvent (Resolved previousRoute route)
          Redirect route -> driver.redirect route

    runRouter' :: i -> Effect Unit
    runRouter' newRoute = do
      previousRoute <- readPreviousRoute
      handleEvent (Routing previousRoute newRoute)
      launchAff_ (runRouter (onCommand newRoute) (onRouteStart previousRoute newRoute))
  pure
    { initialize: driver.initialize runRouter'
    , navigate: driver.navigate
    , redirect: driver.redirect
    }
