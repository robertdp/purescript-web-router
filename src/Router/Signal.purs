module React.Basic.Hooks.Router.Signal
  ( Signal
  , create
  , read
  , write
  , subscribe
  , UseSignal
  , useSignal
  ) where

import Prelude
import Data.Array (deleteBy, snoc)
import Data.Foldable (traverse_)
import Data.Tuple.Nested ((/\))
import Data.Newtype (class Newtype)
import Effect (Effect)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import React.Basic.Hooks (UnsafeReference(..), UseEffect, UseState, Hook)
import React.Basic.Hooks as React
import Unsafe.Reference (unsafeRefEq)

newtype Signal a
  = Signal
  { read :: Effect a
  , write :: a -> Effect Unit
  , subscribe :: (a -> Effect Unit) -> Effect (Effect Unit)
  }

create :: forall a. a -> Effect (Signal a)
create init = ado
  value <- Ref.new init
  subscribers <- Ref.new []
  let
    read = Ref.read value

    write a = do
      Ref.write a value
      Ref.read subscribers >>= traverse_ \k -> k a

    subscribe k = do
      unsubscribing <- Ref.new false
      let
        subscriber = \a -> unlessM (Ref.read unsubscribing) (k a)
      Ref.modify_ (flip snoc subscriber) subscribers
      Ref.read value >>= k
      pure do
        Ref.write true unsubscribing
        Ref.modify_ (deleteBy unsafeRefEq subscriber) subscribers
  in Signal { read, write, subscribe }

read :: forall a. Signal a -> Effect a
read (Signal signal) = signal.read

write :: forall a. Signal a -> a -> Effect Unit
write (Signal signal) = signal.write

subscribe :: forall a. Signal a -> (a -> Effect Unit) -> Effect (Effect Unit)
subscribe (Signal signal) = signal.subscribe

newtype UseSignal a hooks
  = UseSignal (UseEffect (UnsafeReference (Signal a)) (UseState a hooks))

derive instance newtypeUseSignal :: Newtype (UseSignal a hooks) _

useSignal :: forall a. Signal a -> Hook (UseSignal a) a
useSignal signal =
  React.coerceHook React.do
    value /\ setValue <- React.useState $ unsafePerformEffect $ read signal
    React.useEffect (UnsafeReference signal) do subscribe signal (setValue <<< const)
    pure value
