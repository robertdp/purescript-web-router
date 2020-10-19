module Wire.React.Router.Control where

import Prelude
import Control.Monad.Free.Trans (FreeT, liftFreeT)
import Control.Monad.Indexed (class IxApplicative, class IxApply, class IxBind, class IxFunctor, class IxMonad, iap)
import Data.Lens (Lens', Prism', is, lens, prism')
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Type.Equality (class TypeEquals)

data Transition route
  = Transitioning (Maybe route) route
  | Resolved (Maybe route) route

derive instance eqRoute :: Eq route => Eq (Transition route)

_Transition :: forall route. Lens' (Transition route) route
_Transition = lens getter setter
  where
  getter = case _ of
    Transitioning _ route -> route
    Resolved _ route -> route

  setter = case _ of
    Transitioning route _ -> Transitioning route
    Resolved route _ -> Resolved route

_Transitioning :: forall route. Prism' (Transition route) route
_Transitioning =
  prism' (Transitioning Nothing) case _ of
    Transitioning _ route -> Just route
    _ -> Nothing

_Resolved :: forall route. Prism' (Transition route) route
_Resolved =
  prism' (Resolved Nothing) case _ of
    Resolved _ route -> Just route
    _ -> Nothing

isTransitioning :: forall route. Transition route -> Boolean
isTransitioning = is _Transitioning

isResolved :: forall route. Transition route -> Boolean
isResolved = is _Resolved

data Command route a
  = Redirect route
  | Override route
  | Continue

derive instance functorCommand :: Functor (Command route)

data Transitioning

data Resolved

newtype Router route i o a
  = Router (FreeT (Command route) Aff a)

liftCommand :: forall route. Command route Unit -> Router route Transitioning Resolved Unit
liftCommand cmd = Router (liftFreeT cmd)

redirect :: forall route. route -> Router route Transitioning Resolved Unit
redirect = liftCommand <<< Redirect

override :: forall route. route -> Router route Transitioning Resolved Unit
override = liftCommand <<< Override

continue :: forall route. Router route Transitioning Resolved Unit
continue = liftCommand Continue

derive instance newtypeRouter :: Newtype (Router route i o a) _

instance ixFunctorRouter :: IxFunctor (Router route) where
  imap f (Router router) = Router (map f router)

instance ixApplyRouter :: IxApply (Router route) where
  iapply = iap

instance ixBindRouter :: IxBind (Router route) where
  ibind (Router router) f = Router (router >>= \a -> case f a of Router next -> next)

instance ixApplicativeRouter :: IxApplicative (Router route) where
  ipure a = Router (pure a)

instance ixMonadRouter :: IxMonad (Router route)

instance functorRouter :: (TypeEquals Transitioning i, TypeEquals i o) => Functor (Router route i o) where
  map f a = wrap do f <$> unwrap a

instance applyRouter :: (TypeEquals Transitioning i, TypeEquals i o) => Apply (Router route i o) where
  apply = ap

instance applicativeRouter :: (TypeEquals Transitioning i, TypeEquals i o) => Applicative (Router route i o) where
  pure a = Router (pure a)

instance bindRouter :: (TypeEquals Transitioning i, TypeEquals i o) => Bind (Router route i o) where
  bind (Router router) f = Router (router >>= \a -> case f a of Router next -> next)

instance monadRouter :: (TypeEquals Transitioning i, TypeEquals i o) => Monad (Router route i o)

instance monadEffectRouter :: (TypeEquals Transitioning i, TypeEquals i o) => MonadEffect (Router route i o) where
  liftEffect eff = Router (liftEffect eff)

instance monadAffRouter :: (TypeEquals Transitioning i, TypeEquals i o) => MonadAff (Router route i o) where
  liftAff aff = Router (liftAff aff)
