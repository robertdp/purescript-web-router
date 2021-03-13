module Web.Router.Driver.Hash where

import Prelude
import Data.Foldable (class Foldable)
import Routing.Hash as Hash
import Web.Router.Types (Driver(..))

makeDriver :: forall f i o. Foldable f => (String -> f i) -> (o -> String) -> Driver i o
makeDriver parser printer =
  Driver
    { initialize: Hash.matchesWith parser <<< const
    , navigate: Hash.setHash <<< printer
    , redirect: Hash.setHash <<< printer
    }
