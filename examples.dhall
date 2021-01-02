{ name = "wire-react-router-examples"
, dependencies = [ "react-basic-dom", "routing-duplex", "wire-react" ] # (./spago.dhall).dependencies
, packages = ./packages.dhall
, sources = [ "examples/**/*.purs", "src/**/*.purs" ]
}
