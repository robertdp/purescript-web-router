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

type Router route
  = { initialize :: Effect (Effect Unit)
    , navigate :: route -> Effect Unit
    , redirect :: route -> Effect Unit
    }

data Event route
  = Transitioning (Maybe route) route
  | Resolved (Maybe route) route

derive instance eqRoute :: Eq route => Eq (Event route)

data Command :: Type -> Type -> Type -> Type
data Command i o a
  = Redirect i
  | Override o
  | Continue

derive instance functorCommand :: Functor (Command i o)

data Transitioning

data Resolved

newtype Transition :: forall k1 k2. Type -> Type -> k1 -> k2 -> Type -> Type
newtype Transition i o x y a
  = Transition (FreeT (Command i o) Aff a)

derive instance newtypeTransition :: Newtype (Transition i o x y a) _

instance ixFunctorTransition :: IxFunctor (Transition i o) where
  imap f (Transition router) = Transition (map f router)

instance ixApplyTransition :: IxApply (Transition i o) where
  iapply = iap

instance ixApplicativeTransition :: IxApplicative (Transition i o) where
  ipure a = Transition (pure a)

instance ixBindTransition :: IxBind (Transition i o) where
  ibind (Transition router) f = Transition (router >>= \a -> case f a of Transition next -> next)

instance ixMonadTransition :: IxMonad (Transition i o)

instance functorTransition :: (TypeEquals Transitioning x, TypeEquals y Transitioning) => Functor (Transition i o x y) where
  map f (Transition router) = Transition (map f router)

instance applyTransition :: (TypeEquals Transitioning x, TypeEquals y Transitioning) => Apply (Transition i o x y) where
  apply = ap

instance applicativeTransition :: (TypeEquals Transitioning x, TypeEquals y Transitioning) => Applicative (Transition i o x y) where
  pure a = Transition (pure a)

instance bindTransition :: (TypeEquals Transitioning x, TypeEquals y Transitioning) => Bind (Transition i o x y) where
  bind (Transition router) f = Transition (router >>= \a -> case f a of Transition next -> next)

instance monadTransition :: (TypeEquals Transitioning x, TypeEquals y Transitioning) => Monad (Transition i o x y)

instance monadEffectTransition :: (TypeEquals Transitioning x, TypeEquals y Transitioning) => MonadEffect (Transition i o x y) where
  liftEffect eff = Transition (liftEffect eff)

instance monadAffTransition :: (TypeEquals Transitioning x, TypeEquals y Transitioning) => MonadAff (Transition i o x y) where
  liftAff aff = Transition (liftAff aff)
