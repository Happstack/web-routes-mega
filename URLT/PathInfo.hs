{-# LANGUAGE FlexibleInstances #-}
module URLT.PathInfo where

import Control.Applicative.Error (Failing(Failure, Success))
import Control.Monad.Consumer (Consumer, next, runConsumer)
import URLT.Base

class PathInfo a where
  toPathSegments :: a -> [String]
  fromPathSegments :: [String] -> Failing (a, [String])
  
instance PathInfo [String] where  
  toPathSegments = id
  fromPathSegments = Success
  
toPathInfo :: (PathInfo u) => u -> String
toPathInfo = encodePathInfo . toPathSegments

fromPathInfo :: (PathInfo u) => String -> Failing u
fromPathInfo = fromPathSegments . decodePathInfo
