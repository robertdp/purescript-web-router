module React.Basic.Hooks.Router.Control
  ( Transition(..)
  , _Transition
  , _Pending
  , _Completed
  , isPending
  , isCompleted
  , Command(..)
  , Pending
  , Completed
  , Router
  , runRouter
  , redirect
  , override
  , continue
  ) where

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

newtype Router route i o a
  = Router (ExceptT (Command route) Aff a)

runRouter :: forall route. Router route Pending Completed Unit -> Aff (Command route)
runRouter (Router router) = unsafePartial fromLeft <$> runExceptT router

liftCommand :: forall route. Command route -> Router route Pending Completed Unit
liftCommand = wrap <<< except <<< Left

redirect :: forall route. route -> Router route Pending Completed Unit
redirect = liftCommand <<< Redirect

override :: forall route. route -> Router route Pending Completed Unit
override = liftCommand <<< Override

continue :: forall route. Router route Pending Completed Unit
continue = liftCommand Continue

derive instance newtypeRouter :: Newtype (Router route i o a) _

instance ixFunctorRouter :: IxFunctor (Router route) where
  imap f a = wrap do f <$> unwrap a

instance ixApplyRouter :: IxApply (Router route) where
  iapply f a = wrap do unwrap f <*> unwrap a

instance ixBindRouter :: IxBind (Router route) where
  ibind ma f = wrap do unwrap ma >>= unwrap <<< f

instance ixApplicativeRouter :: IxApplicative (Router route) where
  ipure = wrap <<< pure

instance ixMonadRouter :: IxMonad (Router route)

derive instance functorRouter :: Functor (Router route Pending Pending)

instance applyRouter :: Apply (Router route Pending Pending) where
  apply = iapply

instance applicativeRouter :: Applicative (Router route Pending Pending) where
  pure = ipure

instance bindRouter :: Bind (Router route Pending Pending) where
  bind = ibind

instance monadRouter :: Monad (Router route Pending Pending)

instance monadEffectRouter :: MonadEffect (Router route Pending Pending) where
  liftEffect = wrap <<< liftEffect

instance monadAffRouter :: MonadAff (Router route Pending Pending) where
  liftAff = wrap <<< liftAff
