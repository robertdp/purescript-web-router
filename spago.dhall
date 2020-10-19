{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "wire-react-router"
, dependencies =
  [ "aff"
  , "freet"
  , "indexed-monad"
  , "profunctor-lenses"
  , "react-basic-hooks"
  , "routing"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
