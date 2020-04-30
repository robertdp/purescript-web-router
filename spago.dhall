{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies = [ "aff", "indexed-monad", "prelude", "profunctor-lenses", "react-basic-hooks", "refs", "routing", "routing-duplex", "transformers" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
