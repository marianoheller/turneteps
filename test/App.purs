module Test.App (spec) where

import Prelude
import Test.Spec (SpecT, describe, it)
import App (AppF(..))
import App as App
import App.Env as Env
import App.Resources (Resource)
import App.Resources as Resources
import Control.Monad.Free (foldFree)
import Data.Argonaut (class DecodeJson)
import Data.Argonaut as Argonaut
import Data.Date (Date)
import Data.Date as Date
import Data.Either (Either(..))
import Data.Enum (toEnum)
import Data.Maybe (fromJust)
import Data.Newtype (unwrap)
import Data.Traversable (for)
import Dotenv (loadContents) as Dotenv
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Partial.Unsafe (unsafePartial)
import Test.Spec.Assertions (shouldEqual)

mockFetch :: forall a. DecodeJson a => Resource a -> Aff a
mockFetch _ = do
  a <- Argonaut.decodeJson <$> Argonaut.fromString <$> (readTextFile UTF8 "data/login.json")
  case a of
    Left e -> liftEffect $ throw $ show e
    Right v -> pure v

mockDate :: Date
mockDate =
  unsafePartial $ fromJust
    $ do
        y <- toEnum 2000
        m <- toEnum 10
        d <- toEnum 10
        pure $ Date.canonicalDate y m d

appMockI :: AppF ~> Aff
appMockI = case _ of
  (GetEnv next) -> do
    str <- readTextFile UTF8 ".env-test"
    _ <- Dotenv.loadContents str
    testEnv <- liftEffect Env.getEnv
    pure $ next testEnv
  (GetLowerDateBound next) -> do
    pure $ next mockDate
  (Login url auth input next) -> do
    creds <- mockFetch $ Resources.login url auth input
    pure $ next creds
  (GetReservas url creds next) -> do
    reservas <- mockFetch $ Resources.misReservas url creds
    pure $ next reservas
  (GetClases url creds next) -> do
    clases <- mockFetch $ Resources.clases url creds
    pure $ next clases
  (PostReservas url creds clases next) -> do
    results <- for (unwrap clases) (mockFetch <<< Resources.reserva url creds)
    pure $ next results

spec :: forall i. Monad i => SpecT Aff Unit i Unit
spec =
  describe "app" do
    it "feature?" do
      result <- foldFree appMockI App.app
      result `shouldEqual` []
