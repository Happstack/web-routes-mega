module URLT.QuickCheck where

import Control.Monad.Consumer
import Test.QuickCheck
-- import URLT
import URLTH

asURLInverse_prop :: (Eq url, AsURL url) => url -> Bool
asURLInverse_prop url =
    url == (fromURL $ toURLS url "")
