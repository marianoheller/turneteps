module App.Creds (Creds, CredsData, mkCreds) where

import Prelude
import Data.Argonaut (class DecodeJson, decodeJson, (.:))

type CredsData
  = { access_token :: String
    , refresh_token :: String
    , scope :: String
    , token_type :: String
    }

newtype Creds
  = Creds CredsData

instance decodeJsonCreds :: DecodeJson Creds where
  decodeJson json = do
    obj <- decodeJson json
    access_token <- obj .: "access_token"
    refresh_token <- obj .: "refresh_token"
    scope <- obj .: "scope"
    token_type <- obj .: "token_type"
    pure $ Creds { access_token, refresh_token, scope, token_type }

instance showCreds :: Show Creds where
  show (Creds r) = r.token_type <> " " <> r.access_token

mkCreds :: CredsData -> Creds
mkCreds = Creds
