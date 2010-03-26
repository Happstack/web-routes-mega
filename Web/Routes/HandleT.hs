module Web.Routes.HandleT where

import Control.Applicative.Error(Failing(Failure, Success))
import Control.Monad.Reader (ReaderT(runReaderT))
import Data.Monoid (Monoid(mempty, mappend))
import Web.Routes.Monad (RouteT, runRouteT)

data Site url pathInfo a
    = Site { handleLink  :: (url -> pathInfo) -> url -> a
           , defaultPage :: url
           , formatLink  :: url -> pathInfo
           , parseLink   :: pathInfo -> Failing url
           }

withDefault :: (Eq pathInfo, Monoid pathInfo) => Site url pathInfo a -> pathInfo -> Failing url
withDefault site pathInfo
  | pathInfo == mempty = Success (defaultPage site)
  | otherwise          = (parseLink site) pathInfo
                         
runSite :: (Monoid pathInfo, Eq pathInfo) => pathInfo -> Site url pathInfo a -> pathInfo -> (Failing a)
runSite approot site pathInfo =
  case (withDefault site) pathInfo of
    (Failure errs) -> (Failure errs)
    (Success url)  -> Success $ (handleLink site) (\url -> approot `mappend` (formatLink site url)) url
{-  
    let fLink = 
            case linkStr of
                 "" -> Success (defaultPage site)
                 _ -> (parseLink site) linkStr
    in
      case fLink of
        (Failure errs) -> return (Failure errs)
        (Success lnk) -> return . Success =<< runRouteT ((handleLink site) lnk) ((approot ++) . (formatLink site))                         
-}




{-
instance (Functor m) => Functor (Site u l m) where
    fmap f site =
        site { handleLink = \link -> fmap f ((handleLink site) link) }
-}

-- fromPathInfo :: Site 
{-
runSite :: String -> Site link Link a -> Link -> (Failing a)
runSite approot site linkStr =
    let fLink = 
            case linkStr of
                 "" -> Success (defaultPage site)
                 _ -> (parseLink site) linkStr
    in
      case fLink of
        (Failure errs) -> (Failure errs)
        (Success lnk) -> Success $ (handleLink site) ((approot ++) . (formatLink site)) lnk
-}
--         (Success lnk) -> (handleLink site) ((approot ++) . (formatLink site)) lnk
--        (Success lnk) -> return . Success =<< runRouteT ((handleLink site) lnk) ((approot ++) . (formatLink site))

{-
runSite :: (Monad m) => String -> Site link Link m a -> Link -> m (Failing a)
runSite approot site linkStr =
    let fLink = 
            case linkStr of
                 "" -> Success (defaultPage site)
                 _ -> (parseLink site) linkStr
    in
      case fLink of
        (Failure errs) -> return (Failure errs)
        (Success lnk) -> return . Success =<< runRouteT ((handleLink site) lnk) ((approot ++) . (formatLink site))
-}