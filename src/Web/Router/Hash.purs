module Web.Router.Hash where

import Prelude
import Data.Foldable (class Foldable)
import Routing.Hash as Hash
import Web.Router.Internal.Types (DriverInterface)

mkInterface :: forall f i o. Foldable f => (String -> f i) -> (o -> String) -> DriverInterface i o
mkInterface parser printer =
  { initialize: Hash.matchesWith parser <<< const
  , navigate: Hash.setHash <<< printer
  , redirect: Hash.setHash <<< printer
  }
