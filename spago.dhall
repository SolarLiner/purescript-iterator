{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "iterator"
, dependencies =
  [ "console"
  , "effect"
  , "psci-support"
  , "quickcheck"
  , "spec"
  , "spec-quickcheck"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
