{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "argonaut"
  , "console"
  , "dotenv"
  , "effect"
  , "filterable"
  , "formatters"
  , "milkis"
  , "node-process"
  , "now"
  , "psci-support"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
