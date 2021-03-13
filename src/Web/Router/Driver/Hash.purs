module Web.Router.Driver.Hash where

import Prelude
import Data.Foldable (class Foldable)
import Data.Maybe (Maybe)
import Effect (Effect)
import Routing.Hash as Hash
import Web.Router as Router
import Web.Router.Types (Driver(..), Event, Resolved, Transition, Transitioning, Router)

makeDriver :: forall f o i. Foldable f => (String -> f o) -> (i -> String) -> Driver i o
makeDriver parser printer =
  Driver
    { initialize: Hash.matchesWith parser <<< const
    , navigate: Hash.setHash <<< printer
    , redirect: Hash.setHash <<< printer
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
  let
    driver = makeDriver parser printer
  Router.makeRouter onTransition onEvent driver
