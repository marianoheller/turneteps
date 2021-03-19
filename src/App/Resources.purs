module App.Resources where

import Prelude
import App.Data.Clases (Clase(..), Clases)
import App.Data.Creds (BasicAuth, Creds)
import App.Data.Reservas (Reservas)
import App.Env (LoginInput)
import Data.Argonaut (encodeJson, stringify)
import Data.Maybe (Maybe(..))
import Milkis as M
import App.Data.ReservaResult (ReservaResult)
import Unsafe.Coerce (unsafeCoerce)
import Foreign.URLSearchParams as URLSearchParams

type Slot
  = { start :: String, end :: String, status :: String }

type ResourceExtras
  = ( body :: String )

newtype Resource :: forall k. k -> Type
newtype Resource a
  = Resource { method :: M.Method, url :: String, headers :: M.Headers, body :: Maybe String }

toFormData :: LoginInput -> String
toFormData r = URLSearchParams.toString $ URLSearchParams.fromString (unsafeCoerce r)

login :: String -> BasicAuth -> LoginInput -> Resource Creds
login usersUrl basicAuth loginInput =
  Resource
    { method: M.postMethod
    , url: usersUrl <> "/oauth/token"
    , body: Just $ toFormData loginInput
    , headers:
        M.makeHeaders
          { "authorization": show basicAuth
          , "content-type": "application/x-www-form-urlencoded"
          }
    }

misReservas :: String -> Creds -> Resource Reservas
misReservas apiUrl creds =
  Resource
    { method: M.postMethod
    , url: apiUrl <> "/api/service/class/book/list"
    , body: Nothing
    , headers:
        M.makeHeaders
          { "authorization": show creds
          , "content-type": "application/json"
          }
    }

clases :: String -> Creds -> Resource Clases
clases apiUrl creds =
  Resource
    { method: M.postMethod
    , url: apiUrl <> "/api/service/class/club/category/list"
    , body: Just $ stringify $ encodeJson { clubId: 36, categoryId: 3 } -- FIXME: not hardcoded
    , headers:
        M.makeHeaders
          { "authorization": show creds
          , "content-type": "application/json"
          }
    }

reserva :: String -> Creds -> Clase -> Resource ReservaResult
reserva apiUrl creds (Clase clase) =
  Resource
    { method: M.postMethod
    , url: apiUrl <> "/api/service/class/book"
    , body: Just $ stringify $ encodeJson { claseId: clase.claseId }
    , headers:
        M.makeHeaders
          { "authorization": show creds
          , "content-type": "application/json"
          }
    }
