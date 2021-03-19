module Test.Main where

import Effect (Effect)
import Effect.Aff (launchAff_)
import Prelude (Unit, ($))
import Test.App as TestApp
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main =
  launchAff_
    $ runSpec [ consoleReporter ] TestApp.spec
