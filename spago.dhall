{ name = "web-router"
, license = "BSD-3-Clause"
, repository = "https://github.com/robertdp/purescript-web-router.git"
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
, sources = [ "src/**/*.purs" ]
}
