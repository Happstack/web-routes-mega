module URLT.QuickCheck where

import Control.Monad.Consumer
import Test.QuickCheck
import URLT

asURLInverse_prop :: (AsURL url) => url -> Bool
asURLInverse_prop url =
    url == (fromURL $ toURLS url "")
