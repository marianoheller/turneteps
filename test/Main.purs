module Test.Main where

import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (un, wrap)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Prelude (Unit, ($))
import Test.App as TestApp
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpecT)

main :: Effect Unit
main = launchAff_ $ un Identity $ runSpecT testConfig [ consoleReporter ] TestApp.spec
  where
  testConfig = { slow: wrap 5000.0, timeout: Just $ wrap 10000.0, exit: false }
