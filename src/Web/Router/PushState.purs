module Web.Router.PushState where

import Prelude
import Data.Foldable (class Foldable)
import Effect (Effect)
import Foreign (unsafeToForeign)
import Routing.PushState (PushStateInterface)
import Routing.PushState as PushState
import Web.Router.Internal.Types (DriverInterface)

mkInterface :: forall f i o. Foldable f => (String -> f i) -> (o -> String) -> Effect (DriverInterface i o)
mkInterface parser printer = mkInterface_ parser printer <$> PushState.makeInterface

mkInterface_ :: forall f i o. Foldable f => (String -> f i) -> (o -> String) -> PushStateInterface -> DriverInterface i o
mkInterface_ parser printer interface =
  { initialize: \k -> PushState.matchesWith parser (\_ -> k) interface
  , navigate: interface.pushState (unsafeToForeign {}) <<< printer
  , redirect: interface.replaceState (unsafeToForeign {}) <<< printer
  }
