# purescript-wire-react-router

A basic pushstate router for React, with support for asynchronous routing logic. Built using [react-basic-hooks](https://github.com/spicydonuts/purescript-react-basic-hooks) and [wire](https://github.com/robertdp/purescript-wire). I recommend [routing-duplex](https://github.com/natefaubion/purescript-routing-duplex) for easy parsing and printing.

```purescript
makeRouter ::
  forall route f.
  Foldable f =>
  { interface :: PushStateInterface
  , fallback :: route
  , parse :: String -> f route
  , print :: route -> String
  , onRoute :: route -> Router route Transitioning Resolved Unit
  } ->
  Effect
    { signal :: Signal (Route route)
    , component :: JSX
    , navigate :: route -> Effect Unit
    , redirect :: route -> Effect Unit
    }
```

For a basic example see [`examples/RoutingDuplex.purs`](https://github.com/robertdp/purescript-wire-react-router/blob/master/examples/RoutingDuplex.purs).
