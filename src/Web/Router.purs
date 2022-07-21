module Web.Router
  ( module Control
  , module Types
  , mkRouter
  ) where

import Prelude
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, error, killFiber, launchAff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Web.Router.Internal.Control (Resolved, RouterIndex, RouterM, Routing, continue, override, redirect) as Control
import Web.Router.Internal.Control (RouterCommand(..), Resolved, RouterM, Routing, runRouter)
import Web.Router.Internal.Types (Driver, Driver', Router, RouterEvent(..), _Resolved, _RouterEvent, _Routing, isResolved, isRouting) as Types
import Web.Router.Internal.Types (Driver, Router, RouterEvent(..))

mkRouter
  :: forall i o
   . (Maybe i -> i -> RouterM i o Routing Resolved Unit)
  -> (RouterEvent i -> Effect Unit)
  -> Driver i o
  -> Effect (Router o)
mkRouter onNavigation onEvent driver = do
  lastFiberRef <- Ref.new Nothing
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

    onCommand :: forall a. i -> RouterCommand i o a -> Aff Unit
    onCommand newRoute cmd =
      liftEffect do
        previousRoute <- readPreviousRoute
        case cmd of
          Continue -> handleEvent $ Resolved previousRoute newRoute
          Override route -> handleEvent $ Resolved previousRoute route
          Redirect route -> driver.redirect route

    runRouter' :: i -> Effect Unit
    runRouter' newRoute = do
      lastFiber <- Ref.read lastFiberRef
      for_ lastFiber $ killFiber (error "Killing previous routing fiber") >>> launchAff_
      previousRoute <- readPreviousRoute
      handleEvent $ Routing previousRoute newRoute
      newFiber <-
        onNavigation previousRoute newRoute
          # runRouter (onCommand newRoute)
          # launchAff
      Ref.write (Just newFiber) lastFiberRef
  pure
    { initialize: driver.initialize runRouter'
    , navigate: driver.navigate
    , redirect: driver.redirect
    }
