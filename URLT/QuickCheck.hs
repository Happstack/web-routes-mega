module URLT.QuickCheck where

import Control.Applicative.Error
import Control.Monad.Consumer
import Test.QuickCheck
-- import URLT
import URLTH

asURLInverse_prop :: (Eq url, AsURL url) => url -> Bool
asURLInverse_prop url =
    case (fromURL $ toURLS url "") of
      Success url' -> url == url'
      _ -> False
