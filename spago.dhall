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
  , "formatters"
  , "milkis"
  , "node-process"
  , "now"
  , "option"
  , "proxy"
  , "psci-support"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
