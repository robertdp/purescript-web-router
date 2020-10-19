## Module Wire.React.Router.Control

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

#### `_Transition`

``` purescript
_Transition :: forall route. Lens' (Transition route) route
```

#### `_Transitioning`

``` purescript
_Transitioning :: forall route. Prism' (Transition route) route
```

#### `_Resolved`

``` purescript
_Resolved :: forall route. Prism' (Transition route) route
```

#### `isTransitioning`

``` purescript
isTransitioning :: forall route. Transition route -> Boolean
```

#### `isResolved`

``` purescript
isResolved :: forall route. Transition route -> Boolean
```

#### `Command`

``` purescript
data Command route a
  = Redirect route
  | Override route
  | Continue
```

##### Instances
``` purescript
Functor (Command route)
```

#### `Transitioning`

``` purescript
data Transitioning
```

#### `Resolved`

``` purescript
data Resolved
```

#### `Router`

``` purescript
newtype Router route i o a
  = Router (FreeT (Command route) Aff a)
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

#### `liftCommand`

``` purescript
liftCommand :: forall route. Command route Unit -> Router route Transitioning Resolved Unit
```

#### `redirect`

``` purescript
redirect :: forall route. route -> Router route Transitioning Resolved Unit
```

#### `override`

``` purescript
override :: forall route. route -> Router route Transitioning Resolved Unit
```

#### `continue`

``` purescript
continue :: forall route. Router route Transitioning Resolved Unit
```


