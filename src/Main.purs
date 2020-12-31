module Main where

import Prelude
import App.Date as Date
import App.Request as Request
import App.Resources as Resources
import Data.Either (Either(..))
import Data.Traversable (for)
import Dotenv (loadFile) as Dotenv
import Effect (Effect)
import Effect.Aff (Error, runAff_)
import Effect.Class (liftEffect)
import Effect.Console as Console

main :: Effect Unit
main =
  runAff_ handleResult do
    _ <- Dotenv.loadFile
    yesterday <- liftEffect $ Date.getYesterday
    testData <- (map join) $ for (Date.parseDate yesterday) (Request.fetch <<< Resources.misTurnos)
    pure $ show testData

handleResult :: Either Error String -> Effect Unit
handleResult eResult = case eResult of
  Right result -> Console.log result
  Left err -> do
    Console.error "Task could not be finished successfully"
    Console.errorShow err
