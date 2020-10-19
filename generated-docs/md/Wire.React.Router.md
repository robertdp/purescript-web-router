## Module Wire.React.Router

#### `makeRouter`

``` purescript
makeRouter :: forall f route. Foldable f => PushStateInterface -> { onRoute :: route -> Router route Transitioning Resolved Unit, onTransition :: Transition route -> Effect Unit, parse :: String -> f route, print :: route -> String } -> Effect { component :: JSX, navigate :: route -> Effect Unit, redirect :: route -> Effect Unit }
```


### Re-exported from Wire.React.Router.Control:

#### `Transitioning`

``` purescript
data Transitioning
```

#### `Transition`

``` purescript
data Transition route
  = Transitioning (Maybe route) route
  | Resolved (Maybe route) route
```

##### Instances
``` purescript
(Eq route) => Eq (Transition route)
```

#### `Router`

``` purescript
newtype Router route i o a
```

##### Instances
``` purescript
Newtype (Router route i o a) _
IxFunctor (Router route)
IxApply (Router route)
IxBind (Router route)
IxApplicative (Router route)
IxMonad (Router route)
(TypeEquals Transitioning i, TypeEquals i o) => Functor (Router route i o)
(TypeEquals Transitioning i, TypeEquals i o) => Apply (Router route i o)
(TypeEquals Transitioning i, TypeEquals i o) => Applicative (Router route i o)
(TypeEquals Transitioning i, TypeEquals i o) => Bind (Router route i o)
(TypeEquals Transitioning i, TypeEquals i o) => Monad (Router route i o)
(TypeEquals Transitioning i, TypeEquals i o) => MonadEffect (Router route i o)
(TypeEquals Transitioning i, TypeEquals i o) => MonadAff (Router route i o)
```

#### `Resolved`

``` purescript
data Resolved
```

#### `Command`

``` purescript
data Command route a
```

##### Instances
``` purescript
Functor (Command route)
```

#### `redirect`

``` purescript
redirect :: forall route. route -> Router route Transitioning Resolved Unit
```

#### `override`

``` purescript
override :: forall route. route -> Router route Transitioning Resolved Unit
```

#### `isTransitioning`

``` purescript
isTransitioning :: forall route. Transition route -> Boolean
```

#### `isResolved`

``` purescript
isResolved :: forall route. Transition route -> Boolean
```

#### `continue`

``` purescript
continue :: forall route. Router route Transitioning Resolved Unit
```

#### `_Transitioning`

``` purescript
_Transitioning :: forall route. Prism' (Transition route) route
```

#### `_Transition`

``` purescript
_Transition :: forall route. Lens' (Transition route) route
```

#### `_Resolved`

``` purescript
_Resolved :: forall route. Prism' (Transition route) route
```

