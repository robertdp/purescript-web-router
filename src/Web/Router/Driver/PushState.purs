module Web.Router.Driver.PushState where

import Prelude
import Data.Foldable (class Foldable)
import Data.Maybe (Maybe)
import Effect (Effect)
import Foreign (unsafeToForeign)
import Routing.PushState (PushStateInterface)
import Routing.PushState as PushState
import Web.Router as Router
import Web.Router.Types (Driver(..), Event, Resolved, Transition, Transitioning, Router)

makeDriver :: forall f i o. Foldable f => (String -> f o) -> (i -> String) -> Effect (Driver i o)
makeDriver parser printer = makeDriver_ parser printer <$> PushState.makeInterface

makeDriver_ ::
  forall f i o.
  Foldable f =>
  (String -> f o) ->
  (i -> String) ->
  PushStateInterface ->
  Driver i o
makeDriver_ parser printer interface =
  Driver
    { initialize: \k -> PushState.matchesWith parser (\_ -> k) interface
    , navigate: interface.pushState (unsafeToForeign {}) <<< printer
    , redirect: interface.replaceState (unsafeToForeign {}) <<< printer
    }

makeRouter ::
  forall f i o.
  Foldable f =>
  (String -> f o) ->
  (i -> String) ->
  (Maybe o -> o -> Transition i o Transitioning Resolved Unit) ->
  (Event o -> Effect Unit) ->
  Effect (Router i)
makeRouter parser printer onTransition onEvent = do
  driver <- makeDriver parser printer
  Router.makeRouter onTransition onEvent driver
