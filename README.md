# purescript-web-router

A basic pushstate router for React with support for asynchronous routing logic, built using [react-basic-hooks](https://github.com/spicydonuts/purescript-react-basic-hooks). I recommend [routing-duplex](https://github.com/natefaubion/purescript-routing-duplex) for easy parsing and printing.

For a basic example see [here](https://github.com/robertdp/purescript-web-router-example/blob/master/examples/RoutingDuplex.purs).

## How to use with Spago

Add `web-router` to your `packages.dhall`:

```dhall
let additions =
  { web-router =
      { dependencies =
          [ "aff"
          , "effect"
          , "freet"
          , "indexed-monad"
          , "prelude"
          , "profunctor-lenses"
          , "routing"
          ]
      , repo = "https://github.com/robertdp/purescript-web-router.git"
      , version = "v0.3.0"
      }
  }
```

And then install with
`$ spago install web-router`
