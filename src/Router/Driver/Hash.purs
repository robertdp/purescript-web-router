module Routing.Router.Driver.Hash where

import Prelude
import Data.Foldable (class Foldable)
import Routing.Hash as Hash
import Routing.Router.Types (Driver(..))

makeDriver :: forall f o i. Foldable f => (String -> f o) -> (i -> String) -> Driver i o
makeDriver parser printer =
  Driver
    { initialize: Hash.matchesWith parser <<< const
    , navigate: Hash.setHash <<< printer
    , redirect: Hash.setHash <<< printer
    }
