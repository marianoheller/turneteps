module Foreign.URLSearchParams where

import Prelude

foreign import data URLSearchParams :: Type

instance eqURLSearchParams :: Eq URLSearchParams where
  eq a b = toString a == toString b

instance showURLSearchParams :: Show URLSearchParams where
  show = toString

foreign import fromString :: String -> URLSearchParams

foreign import toString :: URLSearchParams -> String
