{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "wire-react-router"
, dependencies =
  [ "aff"
  , "effect"
  , "freet"
  , "indexed-monad"
  , "prelude"
  , "profunctor-lenses"
  , "routing"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
