module App.Data.ReservaResult where

import Prelude
import Data.Argonaut (class DecodeJson, decodeJson, (.:))
import Data.Newtype (class Newtype)

type ReservaResultData
  = { code :: Int
    , message :: String
    }

newtype ReservaResult
  = ReservaResult ReservaResultData

derive instance newTypeReservaResult :: Newtype ReservaResult _

derive instance eqReservaResult :: Eq ReservaResult

instance showReservaResult :: Show ReservaResult where
  show (ReservaResult r) = show r

instance jsonDecodeReservaResult :: DecodeJson ReservaResult where
  decodeJson json = do
    obj <- decodeJson json
    code <- obj .: "code"
    message <- obj .: "message"
    pure $ ReservaResult { code, message }

isSuccess :: ReservaResult -> Boolean
isSuccess (ReservaResult { code }) = code == 0
