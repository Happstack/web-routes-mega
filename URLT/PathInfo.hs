{-# LANGUAGE FlexibleInstances #-}
module URLT.PathInfo where

import Control.Applicative
import Control.Applicative.Error (Failing(Failure, Success))
import Control.Monad.Consumer (Consumer(Consumer), runConsumer)
import URLT.Base

class PathInfo a where
  toPathSegments :: a -> [String]
  fromPathSegments :: Consumer String (Failing a)

instance PathInfo [String] where  
  toPathSegments = id
  fromPathSegments = Consumer $ \c -> (Success c, [])

toPathInfo :: (PathInfo u) => u -> String
toPathInfo = encodePathInfo . toPathSegments

-- should this fail if not all the input was consumed?
fromPathInfo :: (PathInfo u) => String -> Failing u
fromPathInfo pi = fst $ runConsumer (decodePathInfo pi) fromPathSegments 
