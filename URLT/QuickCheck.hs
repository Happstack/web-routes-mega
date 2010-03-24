module URLT.QuickCheck where

import Control.Applicative.Error (Failing(Success))
import URLT.PathInfo (PathInfo, toPathInfo, fromPathInfo)

pathInfoInverse_prop :: (Eq url, PathInfo url) => url -> Bool
pathInfoInverse_prop url =
    case (fromPathInfo $ toPathInfo url) of
      Success url' -> url == url'
      _ -> False
