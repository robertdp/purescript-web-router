{ name = "web-router"
, dependencies =
  [ "aff"
  , "effect"
  , "foldable-traversable"
  , "foreign"
  , "freet"
  , "indexed-monad"
  , "maybe"
  , "prelude"
  , "profunctor-lenses"
  , "refs"
  , "routing"
  , "type-equality"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
