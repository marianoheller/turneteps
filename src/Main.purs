module Main where

import Prelude
import App.Env (getLoginInfo)
import App.Resources as Resources
import App.Request as Request
import Data.Either (Either(..))
import Dotenv (loadFile) as Dotenv
import Effect (Effect)
import Effect.Aff (Error, runAff_)
import Effect.Class (liftEffect)
import Effect.Console as Console

main :: Effect Unit
main =
  let
    app = do
      _ <- Dotenv.loadFile
      loginInput <- liftEffect getLoginInfo
      testData <- Request.fetch $ Resources.login loginInput
      pure $ show testData
  in
    runAff_ handleResult app

handleResult :: Either Error String -> Effect Unit
handleResult eResult = case eResult of
  Right result -> Console.log "result"
  Left err -> do
    Console.error "Task could not be finished successfully"
    Console.errorShow err
