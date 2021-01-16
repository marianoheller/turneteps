module App.Resources where

import Prelude

import App.Data.Creds (BasicAuth, Creds)
import App.Data.Reservas (Reservas)
import App.Env (LoginInput)
import Milkis as M
import Option as Option
import Unsafe.Coerce (unsafeCoerce)
import Web.URL.URLSearchParams as SearchParams

type Slot
  = { start :: String, end :: String, status :: String }

type ResourceBase
  = ( method :: M.Method, url :: String, headers :: M.Headers )

type ResourceExtras
  = ( body :: String )

newtype Resource a
  = Resource (Option.Record ResourceBase ResourceExtras)

toFormData :: LoginInput -> String
toFormData r = SearchParams.toString $ SearchParams.fromString (unsafeCoerce r)

login :: BasicAuth -> LoginInput -> Resource Creds
login basicAuth loginInput =
  Resource
    $ Option.recordFromRecord
        { method: M.postMethod
        , url: "https://users.megatlon.com.ar/oauth/token"
        , body: toFormData loginInput
        , headers:
            M.makeHeaders
              { "authorization": show basicAuth
              , "content-type": "application/x-www-form-urlencoded"
              }
        }

misReservas :: Creds -> Resource Reservas
misReservas creds =
  Resource
    $ Option.recordFromRecord
        { method: M.postMethod
        , url: "https://classes.megatlon.com.ar/api/service/class/book/list"
        , headers:
            M.makeHeaders
              { "authorization": show creds
              , "content-type": "application/json"
              }
        }
