module Web.Router.Driver.PushState where

import Prelude
import Data.Foldable (class Foldable)
import Effect (Effect)
import Foreign (unsafeToForeign)
import Routing.PushState (PushStateInterface)
import Routing.PushState as PushState
import Web.Router.Types (Driver(..))

makeDriver :: forall f i o. Foldable f => (String -> f i) -> (o -> String) -> Effect (Driver i o)
makeDriver parser printer = makeDriver_ parser printer <$> PushState.makeInterface

makeDriver_ :: forall f i o. Foldable f => (String -> f i) -> (o -> String) -> PushStateInterface -> Driver i o
makeDriver_ parser printer interface =
  Driver
    { initialize: \k -> PushState.matchesWith parser (\_ -> k) interface
    , navigate: interface.pushState (unsafeToForeign {}) <<< printer
    , redirect: interface.replaceState (unsafeToForeign {}) <<< printer
    }
