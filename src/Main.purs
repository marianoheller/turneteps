module Main where

import Prelude
import App.Date as Date
import App.Request (fetch)
import App.Resources as Resources
import Data.Maybe (maybe)
import Data.String as String
import Data.Traversable (for)
import Dotenv (loadFile) as Dotenv
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Node.Process (lookupEnv)

getIdsFromEnv :: Effect (Array String)
getIdsFromEnv = do
  ids <- lookupEnv "appids"
  pure $ maybe [] (String.split (String.Pattern ",")) ids

main :: Effect Unit
main =
  launchAff_ do
    _ <- Dotenv.loadFile
    yesterday <- liftEffect $ Date.getYesterday
    testData <- (map join) $ for (Date.parseDate yesterday) (fetch <<< Resources.misTurnos)
    liftEffect $ log $ show testData
    liftEffect $ log "🍝"

-- for' :: forall a b m t. Functor m => Bind t => Applicative m ⇒ Traversable t ⇒ t a → (a → m (t b)) → m (t b)
-- for' d = (map join) <<< for d
