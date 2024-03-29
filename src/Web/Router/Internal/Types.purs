module Web.Router.Internal.Types where

import Prelude
import Data.Lens (Lens', Prism', is, lens, prism')
import Data.Maybe (Maybe(..))
import Effect (Effect)

type RouterInterface route =
  { initialize :: Effect (Effect Unit)
  , navigate :: route -> Effect Unit
  , redirect :: route -> Effect Unit
  }

type DriverInterface i o =
  { initialize :: (i -> Effect Unit) -> Effect (Effect Unit)
  , navigate :: o -> Effect Unit
  , redirect :: o -> Effect Unit
  }

type DriverInterface' route = DriverInterface route route

data RouterEvent route
  = Routing (Maybe route) route
  | Resolved (Maybe route) route

derive instance Eq route => Eq (RouterEvent route)

_RouterEvent :: forall route. Lens' (RouterEvent route) route
_RouterEvent = lens getter setter
  where
  getter = case _ of
    Routing _ route -> route
    Resolved _ route -> route

  setter = case _ of
    Routing route _ -> Routing route
    Resolved route _ -> Resolved route

_Routing :: forall route. Prism' (RouterEvent route) route
_Routing =
  prism' (Routing Nothing) case _ of
    Routing _ route -> Just route
    _ -> Nothing

_Resolved :: forall route. Prism' (RouterEvent route) route
_Resolved =
  prism' (Resolved Nothing) case _ of
    Resolved _ route -> Just route
    _ -> Nothing

isRouting :: forall route. RouterEvent route -> Boolean
isRouting = is _Routing

isResolved :: forall route. RouterEvent route -> Boolean
isResolved = is _Resolved
