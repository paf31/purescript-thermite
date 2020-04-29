{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "thermite"
, dependencies = [ "aff", "coroutines", "freet", "profunctor-lenses", "react" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
