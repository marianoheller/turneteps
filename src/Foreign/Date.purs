module Foreign.Date (subDays) where

import Data.JSDate (JSDate)

foreign import subDays :: Int -> JSDate -> JSDate
