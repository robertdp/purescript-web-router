# purescript-wire-react-router

A basic pushstate router for React with support for asynchronous routing logic, built using [react-basic-hooks](https://github.com/spicydonuts/purescript-react-basic-hooks). I recommend [routing-duplex](https://github.com/natefaubion/purescript-routing-duplex) for easy parsing and printing and [wire](https://github.com/robertdp/purescript-wire) for easy reactive state.

```purescript
makeRouter ::
  forall route f.
  Foldable f =>
  PushStateInterface ->
  { parse :: String -> f route
  , print :: route -> String
  , onRoute :: route -> Router route Transitioning Resolved Unit
  , onTransition :: Transition route -> Effect Unit
  } ->
  Effect
    { component :: JSX
    , navigate :: route -> Effect Unit
    , redirect :: route -> Effect Unit
    }
```

For a basic example see [`examples/RoutingDuplex.purs`](https://github.com/robertdp/purescript-wire-react-router/blob/master/examples/RoutingDuplex.purs).
