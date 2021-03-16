module Main where

import Prelude
import App (app, appI)
import Control.Monad.Free (foldFree)
import Control.Promise (Promise, fromAff)
import Dotenv (loadFile) as Dotenv
import Effect (Effect)
import Effect.Aff (Aff)

main :: Effect (Promise String)
main =
  let
    task :: Aff String
    task = do
      _ <- Dotenv.loadFile
      results <- foldFree appI app
      pure $ show results
  in
    fromAff task
