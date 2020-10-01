## Module Wire.React.Router

#### `makeRouter`

``` purescript
makeRouter :: forall route f. Foldable f => { decode :: String -> f route, default :: route, encode :: route -> String, interface :: PushStateInterface, onRouteChange :: route -> Router route Transitioning Resolved Unit } -> Effect { component :: JSX, navigate :: route -> Effect Unit, redirect :: route -> Effect Unit, signal :: Signal (Route route) }
```


### Re-exported from Wire.React.Router.Control:

#### `Transitioning`

``` purescript
data Transitioning
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

#### `Route`

``` purescript
data Route route
  = Transitioning (Maybe route) route
  | Resolved (Maybe route) route
```

##### Instances
``` purescript
(Eq route) => Eq (Route route)
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
isTransitioning :: forall route. Route route -> Boolean
```

#### `isResolved`

``` purescript
isResolved :: forall route. Route route -> Boolean
```

#### `continue`

``` purescript
continue :: forall route. Router route Transitioning Resolved Unit
```

#### `_Transitioning`

``` purescript
_Transitioning :: forall route. Prism' (Route route) route
```

#### `_Route`

``` purescript
_Route :: forall route. Lens' (Route route) route
```

#### `_Resolved`

``` purescript
_Resolved :: forall route. Prism' (Route route) route
```

