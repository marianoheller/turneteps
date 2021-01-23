module Foreign.Date (subDays, addDays) where

import Data.JSDate (JSDate)

foreign import subDays :: Int -> JSDate -> JSDate

foreign import addDays :: Int -> JSDate -> JSDate
