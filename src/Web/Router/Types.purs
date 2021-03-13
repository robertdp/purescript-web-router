module Web.Router.Types where

import Prelude
import Control.Monad.Free.Trans (FreeT)
import Control.Monad.Indexed (class IxApplicative, class IxApply, class IxBind, class IxFunctor, class IxMonad, iap)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Type.Equality (class TypeEquals)

newtype Driver :: Type -> Type -> Type
newtype Driver i o
  = Driver
  { initialize :: (o -> Effect Unit) -> Effect (Effect Unit)
  , navigate :: i -> Effect Unit
  , redirect :: i -> Effect Unit
  }

type Driver' route
  = Driver route route

data Transition route
  = Transitioning (Maybe route) route
  | Resolved (Maybe route) route

derive instance eqRoute :: Eq route => Eq (Transition route)

data Command :: Type -> Type -> Type -> Type
data Command i o a
  = Redirect i
  | Override o
  | Continue

derive instance functorCommand :: Functor (Command i o)

data Transitioning

data Resolved

newtype Router :: forall k1 k2. Type -> Type -> k1 -> k2 -> Type -> Type
newtype Router i o x y a
  = Router (FreeT (Command i o) Aff a)

derive instance newtypeRouter :: Newtype (Router i o x y a) _

instance ixFunctorRouter :: IxFunctor (Router i o) where
  imap f (Router router) = Router (map f router)

instance ixApplyRouter :: IxApply (Router i o) where
  iapply = iap

instance ixApplicativeRouter :: IxApplicative (Router i o) where
  ipure a = Router (pure a)

instance ixBindRouter :: IxBind (Router i o) where
  ibind (Router router) f = Router (router >>= \a -> case f a of Router next -> next)

instance ixMonadRouter :: IxMonad (Router i o)

instance functorRouter :: (TypeEquals Transitioning x, TypeEquals y Transitioning) => Functor (Router i o x y) where
  map f (Router router) = Router (map f router)

instance applyRouter :: (TypeEquals Transitioning x, TypeEquals y Transitioning) => Apply (Router i o x y) where
  apply = ap

instance applicativeRouter :: (TypeEquals Transitioning x, TypeEquals y Transitioning) => Applicative (Router i o x y) where
  pure a = Router (pure a)

instance bindRouter :: (TypeEquals Transitioning x, TypeEquals y Transitioning) => Bind (Router i o x y) where
  bind (Router router) f = Router (router >>= \a -> case f a of Router next -> next)

instance monadRouter :: (TypeEquals Transitioning x, TypeEquals y Transitioning) => Monad (Router i o x y)

instance monadEffectRouter :: (TypeEquals Transitioning x, TypeEquals y Transitioning) => MonadEffect (Router i o x y) where
  liftEffect eff = Router (liftEffect eff)

instance monadAffRouter :: (TypeEquals Transitioning x, TypeEquals y Transitioning) => MonadAff (Router i o x y) where
  liftAff aff = Router (liftAff aff)
