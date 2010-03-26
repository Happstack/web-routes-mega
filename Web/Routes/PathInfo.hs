{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module Web.Routes.PathInfo where

import Control.Applicative.Error (Failing(Failure, Success))
import Control.Monad.Consumer (Consumer(Consumer), runConsumer, next)
import Control.Monad (msum)
import Data.List (stripPrefix, tails)
import Data.Maybe (fromJust)
import Web.Routes.Base (decodePathInfo, encodePathInfo)

-- this is not very efficient. Among other things, we need only consider the last 'n' characters of x where n == length y.
stripOverlap :: (Eq a) => [a] -> [a] -> [a]
stripOverlap x y = fromJust $ msum $ [ stripPrefix p y | p <- tails x]

class PathInfo a where
  toPathSegments :: a -> [String]
  fromPathSegments :: Consumer String (Failing a)

toPathInfo :: (PathInfo u) => u -> String
toPathInfo = ('/' :) . encodePathInfo . toPathSegments

-- should this fail if not all the input was consumed?  
--
-- in theory we
-- require the pathInfo to have the initial '/', but this code will
-- still work if it is missing.
--

-- If there are multiple //// at the beginning, we only drop the first
-- one, because we only added one in toPathInfo. Hence the others
-- should be significant.
--
-- However, if the pathInfo was prepend with http://example.org/ with
-- a trailing slash, then things might not line up.
fromPathInfo :: (PathInfo u) => String -> Failing u
fromPathInfo pi = fst $ runConsumer (decodePathInfo $ dropSlash pi) fromPathSegments 
  where
    dropSlash ('/':rs) = rs
    dropSlash x        = x

-- it's instances all the way down

instance PathInfo [String] where  
  toPathSegments = id
  fromPathSegments = Consumer $ \c -> (Success c, [])
  
instance PathInfo String where
  toPathSegments = (:[])
  fromPathSegments = 
    do mStr <- next
       case mStr of
         Nothing    -> return $ Failure ["Expected a String but get end of input."]
         (Just str) -> return $ Success str
  
instance PathInfo Int where  
  toPathSegments i = [show i]
  fromPathSegments =
    do mStr <- next
       case mStr of
         Nothing -> return $ Failure ["Expected an Int but got end of input."]
         (Just str) ->
           case reads str of
             [(n,[])] -> return (Success n)
             _ -> return $ Failure ["Expected an Int but got '" ++ str ++"'"]
