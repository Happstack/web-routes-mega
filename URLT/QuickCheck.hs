module URLT.QuickCheck where

import Control.Applicative.Error (Failing(Success))
import URLT.AsURL (AsURL(toURLS), fromURL)

asURLInverse_prop :: (Eq url, AsURL url) => url -> Bool
asURLInverse_prop url =
    case (fromURL $ toURLS url "") of
      Success url' -> url == url'
      _ -> False
