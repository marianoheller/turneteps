module Main where

import Prelude
import App.Date as Date
import App.Request (fetch)
import App.Resources as Resources
import Data.Traversable (for)
import Dotenv (loadFile) as Dotenv
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)


{- 
TODO:
- Aff error handling (con url invalida crashea todo, esta bien???)
- Env not reading correctly
 -}
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
