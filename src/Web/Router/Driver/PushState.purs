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

type RouterSpec f i o
  = { parser :: String -> f o
    , printer :: i -> String
    , onTransition :: Maybe o -> o -> Transition i o Transitioning Resolved Unit
    , onEvent :: Event o -> Effect Unit
    }

makeRouter :: forall f i o. Foldable f => RouterSpec f i o -> Effect (Router i)
makeRouter { parser, printer, onTransition, onEvent } = do
  driver <- makeDriver parser printer
  Router.makeRouter { driver, onTransition, onEvent }
