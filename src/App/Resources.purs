module App.Resources where

import Prelude
import App.Data.Clases (Clase(..), Clases)
import App.Data.Creds (BasicAuth, Creds)
import App.Data.Reservas (Reservas)
import App.Env (LoginInput)
import Data.Argonaut (encodeJson, stringify)
import Milkis as M
import Option as Option
import ReservaResult (ReservaResult)
import Unsafe.Coerce (unsafeCoerce)
import Web.URL.URLSearchParams as SearchParams

-- TODO: move baseurl to env
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

clases :: Creds -> Resource Clases
clases creds =
  Resource
    $ Option.recordFromRecord
        { method: M.postMethod
        , url: "https://classes.megatlon.com.ar/api/service/class/club/list"
        , body: stringify $ encodeJson { clubId: 36 } -- FIXME: not hardcoded
        , headers:
            M.makeHeaders
              { "authorization": show creds
              , "content-type": "application/json"
              }
        }

reserva :: Creds -> Clase -> Resource ReservaResult
reserva creds (Clase clase) =
  Resource
    $ Option.recordFromRecord
        { method: M.postMethod
        , url: "https://classes.megatlon.com.ar/api/service/class/book"
        , body: stringify $ encodeJson { claseId: clase.claseId }
        , headers:
            M.makeHeaders
              { "authorization": show creds
              , "content-type": "application/json"
              }
        }
