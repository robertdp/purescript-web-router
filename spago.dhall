{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "aff"
  , "indexed-monad"
  , "freet"
  , "profunctor-lenses"
  , "react-basic-hooks"
  , "routing"
  , "wire"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
