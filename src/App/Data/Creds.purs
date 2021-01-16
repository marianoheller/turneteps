module App.Data.Creds (Creds, CredsData, BasicAuth, mkBasicAuth) where

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

newtype BasicAuth
  = BasicAuth String

instance showBasicAuth :: Show BasicAuth where
  show (BasicAuth str) = "Basic " <> str

mkBasicAuth :: String -> BasicAuth
mkBasicAuth = BasicAuth
