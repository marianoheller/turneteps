module App.Resources where

import Prelude
import App.Creds (Creds, CredsData)
import App.Env (LoginInput)
import Milkis as M
import Option as Option
import Unsafe.Coerce (unsafeCoerce)
import Web.URL.URLSearchParams as SearchParams

type ClaseId
  = String

type Slot
  = { start :: String, end :: String, status :: String }

type ResourceBase
  = ( method :: M.Method, url :: String )

type ResourceExtras
  = ( body :: String )

newtype Resource a
  = Resource (Option.Record ResourceBase ResourceExtras)

toFormData :: LoginInput -> String
toFormData r = SearchParams.toString $ SearchParams.fromString (unsafeCoerce r)

login :: LoginInput -> Resource CredsData
login loginInput =
  Resource
    $ Option.recordFromRecord
        { method: M.postMethod
        , url: "https://users.megatlon.com.ar/oauth/token"
        , body: toFormData loginInput
        }

misReservas :: Creds -> Resource String
misReservas _ =
  Resource
    $ Option.recordFromRecord
        { method: M.getMethod
        , url: "/user/reservation?max=10&offset=0&dateFrom="
        }
