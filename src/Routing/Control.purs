module React.Basic.Hooks.Routing.Control where

import Prelude
import Control.Monad.Except (ExceptT, except, runExceptT)
import Control.Monad.Indexed (class IxApplicative, class IxApply, class IxBind, class IxFunctor, class IxMonad, iapply, ibind, ipure)
import Data.Either (Either(..), fromLeft)
import Data.Lens (Lens', Prism', is, lens, prism')
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Partial.Unsafe (unsafePartial)

data Transition route
  = Pending (Maybe route) route
  | Completed (Maybe route) route

_Transition :: forall route. Lens' (Transition route) route
_Transition = lens getter setter
  where
  getter = case _ of
    Pending _ route -> route
    Completed _ route -> route

  setter = case _ of
    Pending route _ -> Pending route
    Completed route _ -> Completed route

_Pending :: forall route. Prism' (Transition route) route
_Pending =
  prism' (Pending Nothing) case _ of
    Pending _ route -> Just route
    _ -> Nothing

_Completed :: forall route. Prism' (Transition route) route
_Completed =
  prism' (Completed Nothing) case _ of
    Completed _ route -> Just route
    _ -> Nothing

isPending :: forall route. Transition route -> Boolean
isPending = is _Pending

isCompleted :: forall route. Transition route -> Boolean
isCompleted = is _Completed

data Command route
  = Redirect route
  | Override route
  | Continue

data Pending

data Completed

newtype Routing route i o a
  = Routing (ExceptT (Command route) Aff a)

runRouting :: forall route. Routing route Pending Completed Unit -> Aff (Command route)
runRouting (Routing routing) = unsafePartial fromLeft <$> runExceptT routing

liftCommand :: forall route. Command route -> Routing route Pending Completed Unit
liftCommand = wrap <<< except <<< Left

redirect :: forall route. route -> Routing route Pending Completed Unit
redirect = liftCommand <<< Redirect

override :: forall route. route -> Routing route Pending Completed Unit
override = liftCommand <<< Override

continue :: forall route. Routing route Pending Completed Unit
continue = liftCommand Continue

derive instance newtypeRouting :: Newtype (Routing route i o a) _

instance ixFunctorRouting :: IxFunctor (Routing route) where
  imap f a = wrap do f <$> unwrap a

instance ixApplyRouting :: IxApply (Routing route) where
  iapply f a = wrap do unwrap f <*> unwrap a

instance ixBindRouting :: IxBind (Routing route) where
  ibind ma f = wrap do unwrap ma >>= unwrap <<< f

instance ixApplicativeRouting :: IxApplicative (Routing route) where
  ipure = wrap <<< pure

instance ixMonadRouting :: IxMonad (Routing route)

derive instance functorRouting :: Functor (Routing route Pending Pending)

instance applyRouting :: Apply (Routing route Pending Pending) where
  apply = iapply

instance applicativeRouting :: Applicative (Routing route Pending Pending) where
  pure = ipure

instance bindRouting :: Bind (Routing route Pending Pending) where
  bind = ibind

instance monadRouting :: Monad (Routing route Pending Pending)

instance monadEffectRouting :: MonadEffect (Routing route Pending Pending) where
  liftEffect = wrap <<< liftEffect

instance monadAffRouting :: MonadAff (Routing route Pending Pending) where
  liftAff = wrap <<< liftAff
