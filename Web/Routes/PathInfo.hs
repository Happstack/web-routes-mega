{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module Web.Routes.PathInfo where

import Control.Applicative.Error (Failing(Failure, Success))
import Control.Monad.Consumer (Consumer(Consumer), runConsumer, next)
import Control.Monad (msum)
import Data.List (stripPrefix, tails)
import Data.Maybe (fromJust)
import URLT.Base (decodePathInfo, encodePathInfo)

-- this is not very efficient. Among other things, we need only consider the last 'n' characters of x where n == length y.
stripOverlap :: (Eq a) => [a] -> [a] -> [a]
stripOverlap x y = fromJust $ msum $ [ stripPrefix p y | p <- tails x]

type URLParser a = GenParser String () a

segment :: String -> URLParser String
segment x = pToken (const x) (\y -> if x == y then Just x else Nothing)


anySegment :: URLParser String
anySegment = pToken (const "any string") Just

pToken msg f = do pos <- getPosition
                  token id (const pos) f

class PathInfo a where
  toPathSegments :: a -> [String]
  fromPathSegments :: URLParser a

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

showParseError = showErrorMessages "or" "unknown parse error" 
                                   "expecting" "unexpected" "end of input"
                                   . errorMessages


-- it's instances all the way down

instance PathInfo [String] where  
  toPathSegments = id
  fromPathSegments = many anySegment
  
instance PathInfo String where
  toPathSegments = (:[])
  fromPathSegments = anySegment
  
instance PathInfo Int where  
  toPathSegments i = [show i]
  fromPathSegments = pToken (const "integer") checkInt
   where checkInt str = 
           case reads str of
             [(n,[])] -> Just n
             _ ->        Nothing
