{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances, UndecidableInstances, PackageImports #-}
module Web.Routes.Happstack where

import Control.Applicative ((<$>))
import Control.Monad (MonadPlus(mzero))
import Data.List (intercalate)
import Happstack.Server (FilterMonad(..), ServerMonad(..), WebMonad(..), HasRqData(..), ServerPartT, Response, Request(rqPaths), ToMessage(..), dirs, seeOther)
import Web.Routes.RouteT (RouteT(RouteT), ShowURL, URL, showURL, liftRouteT, mapRouteT)
import Web.Routes.Site (Site, runSite)

instance (ServerMonad m) => ServerMonad (RouteT url m) where
    askRq       = liftRouteT askRq
    localRq f m = mapRouteT (localRq f) m

instance (FilterMonad a m)=> FilterMonad a (RouteT url m) where
    setFilter     = liftRouteT . setFilter
    composeFilter = liftRouteT . composeFilter
    getFilter     = mapRouteT getFilter 

instance (WebMonad a m) => WebMonad a (RouteT url m) where
    finishWith = liftRouteT . finishWith

instance (HasRqData m) => HasRqData (RouteT url m) where
    askRqEnv       = liftRouteT askRqEnv
    localRqEnv f m = mapRouteT (localRqEnv f) m
    rqDataError    = liftRouteT . rqDataError

implSite :: (Functor m, Monad m, MonadPlus m, ServerMonad m) => String -> FilePath -> Site url (m a) -> m a
implSite domain approot siteSpec =
  do r <- implSite_ domain approot siteSpec
     case r of
       (Left _) -> mzero
       (Right a) -> return a

implSite_ :: (Functor m, Monad m, MonadPlus m, ServerMonad m) => String -> FilePath -> Site url (m a) -> m (Either String a)
implSite_ domain approot siteSpec =
    dirs approot $ do rq <- askRq
                      let pathInfo = intercalate "/" (rqPaths rq)
                          f        = runSite (domain ++ approot) siteSpec pathInfo
                      case f of
                        (Left parseError) -> return (Left parseError)
                        (Right sp)   -> Right <$> (localRq (const $ rq { rqPaths = [] }) sp)
{- 
implSite__ :: (Monad m) => String -> FilePath -> ([ErrorMsg] -> ServerPartT m a) -> Site url (ServerPartT m a) -> (ServerPartT m a)
implSite__ domain approot handleError siteSpec =
    dirs approot $ do rq <- askRq
                      let pathInfo = intercalate "/" (rqPaths rq)
                          f        = runSite (domain ++ approot) siteSpec pathInfo
                      case f of
                        (Failure errs) -> handleError errs
                        (Success sp)   -> localRq (const $ rq { rqPaths = [] }) sp
-}
-- | similar to 'seeOther' but takes a 'URL' 'm' as an argument
seeOtherURL :: (ShowURL m, FilterMonad Response m) => URL m -> m Response
seeOtherURL url = 
    do otherURL <- showURL url
       seeOther otherURL (toResponse "")
