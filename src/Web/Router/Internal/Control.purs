module Web.Router.Internal.Control where

import Prelude

import Control.Applicative.Indexed (class IxApplicative, iapply, imap, ipure)
import Control.Apply.Indexed (class IxApply)
import Control.Bind.Indexed (class IxBind, ibind)
import Control.Monad.Free.Trans (FreeT, liftFreeT, runFreeT)
import Control.Monad.Indexed (class IxMonad, iap)
import Data.Functor.Indexed (class IxFunctor)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Type.Equality (class TypeEquals)

data RouterCommand :: Type -> Type -> Type -> Type
data RouterCommand i o a
  = Continue
  | Override i
  | Redirect o

derive instance Functor (RouterCommand i o)

data RouterIndex

foreign import data Routing :: RouterIndex
foreign import data Resolved :: RouterIndex

newtype RouterM :: Type -> Type -> RouterIndex -> RouterIndex -> Type -> Type
newtype RouterM i o x y a = RouterM (FreeT (RouterCommand i o) Aff a)

instance IxFunctor (RouterM i o) where
  imap f (RouterM r) = RouterM (map f r)

instance IxApply (RouterM i o) where
  iapply = iap

instance IxApplicative (RouterM i o) where
  ipure a = RouterM (pure a)

instance IxBind (RouterM i o) where
  ibind (RouterM r) f = RouterM (r >>= \a -> case f a of RouterM r' -> r')

instance IxMonad (RouterM i o)

instance TypeEquals Routing x => Functor (RouterM i o x x) where
  map = imap

instance TypeEquals Routing x => Apply (RouterM i o x x) where
  apply = iapply

instance TypeEquals Routing x => Applicative (RouterM i o x x) where
  pure = ipure

instance TypeEquals Routing x => Bind (RouterM i o x x) where
  bind = ibind

instance TypeEquals Routing x => Monad (RouterM i o x x)

instance TypeEquals Routing x => MonadEffect (RouterM i o x x) where
  liftEffect eff = RouterM (liftEffect eff)

instance TypeEquals Routing x => MonadAff (RouterM i o x x) where
  liftAff aff = RouterM (liftAff aff)

runRouter :: forall i o. (forall a. RouterCommand i o a -> Aff Unit) -> RouterM i o Routing Resolved Unit -> Aff Unit
runRouter handleCmd (RouterM router) = runFreeT (\cmd -> handleCmd cmd *> mempty) router

continue :: forall i o. RouterM i o Routing Resolved Unit
continue = RouterM (liftFreeT Continue)

override :: forall i o. i -> RouterM i o Routing Resolved Unit
override route = RouterM (liftFreeT (Override route))

redirect :: forall i o. o -> RouterM i o Routing Resolved Unit
redirect route = RouterM (liftFreeT (Redirect route))
