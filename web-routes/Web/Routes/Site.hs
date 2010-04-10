module Web.Routes.Site where

import Data.Maybe (isJust, fromJust)
import Data.Monoid (Monoid(mappend))
import Web.Routes.Base (decodePathInfo, encodePathInfo)

{-|

A site groups together the three functions necesary to make an application:

* A function to convert from the URL type to path segments.

* A function to convert from path segments to the URL, if possible.

* A function to return the application for a given URL.

There are two type parameters for Site: the first is the URL datatype, the
second is the application datatype. The application datatype will depend upon
your server backend.
-}
data Site url a
    = Site {
           {-|
               Return the appropriate application for a given URL.

               The first argument is a function which will give an appropriate
               URL (as a String) for a URL datatype. This is usually
               constructed by a combination of 'formatPathSegments' and the
               prepending of an absolute application root.

               Well behaving applications should use this function to
               generating all internal URLs.
           -}
             handleSite         :: (url -> String) -> url -> a
           , defaultPage        :: Maybe url
           -- | This function must be the inverse of 'parsePathSegments'.
           , formatPathSegments :: url -> [String]
           -- | This function must be the inverse of 'formatPathSegments'.
           , parsePathSegments  :: [String] -> Either String url
           }

instance Functor (Site url) where
  fmap f site = site { handleSite = \showFn u -> f (handleSite site showFn u) }

withDefault :: Site url a -> String -> Either String url
withDefault site pathInfo
  | null pathInfo && isJust (defaultPage site) = Right (fromJust (defaultPage site))
  | otherwise     = (parsePathSegments site) (decodePathInfo pathInfo)
                         
runSite :: String -> Site url a -> String -> (Either String a)
runSite approot site pathInfo =
  case (withDefault site) pathInfo of
    (Left errs) -> (Left errs)
    (Right url)  -> Right $ (handleSite site) (\url -> approot ++ (encodePathInfo $ formatPathSegments site url)) url