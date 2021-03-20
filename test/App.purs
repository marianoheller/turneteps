module Test.App (spec) where

import Prelude
import App (AppF(..))
import App as App
import App.Data.Clases (Clases)
import App.Data.Creds (Creds)
import App.Data.ReservaResult (ReservaResult)
import App.Data.Reservas (Reservas)
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
import Data.String (joinWith)
import Data.Traversable (for)
import Dotenv (loadContents) as Dotenv
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Partial.Unsafe (unsafePartial)
import Test.Spec (SpecT, describe, it)
import Test.Spec.Assertions (shouldEqual)

mockFetch_ :: forall a. DecodeJson a => String -> Resource a -> Aff a
mockFetch_ path _ = do
  fileStr <- readTextFile UTF8 path
  case Argonaut.decodeJson =<< Argonaut.parseJson fileStr of
    Left e -> liftEffect $ throw $ joinWith "\n" [ path, show e ]
    Right v -> pure v

mockFetchLogin :: Resource Creds -> Aff Creds
mockFetchLogin r = mockFetch_ "data/login.json" r

mockFetchReservas :: Resource Reservas -> Aff Reservas
mockFetchReservas r = mockFetch_ "data/reservas.json" r

mockFetchClases :: Resource Clases -> Aff Clases
mockFetchClases r = mockFetch_ "data/classes.json" r

mockPostReserva :: Resource ReservaResult -> Aff ReservaResult
mockPostReserva r = mockFetch_ "data/reservaSuccess.json" r

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
    creds <- mockFetchLogin $ Resources.login url auth input
    pure $ next creds
  (GetReservas url creds next) -> do
    reservas <- mockFetchReservas $ Resources.misReservas url creds
    pure $ next reservas
  (GetClases url creds next) -> do
    clases <- mockFetchClases $ Resources.clases url creds
    pure $ next clases
  (PostReservas url creds clases next) -> do
    results <- for (unwrap clases) (mockPostReserva <<< Resources.reserva url creds)
    pure $ next results

spec :: forall i. Monad i => SpecT Aff Unit i Unit
spec =
  describe "app" do
    it "feature?" do
      result <- foldFree appMockI App.app
      result `shouldEqual` []
