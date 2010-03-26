{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances, UndecidableInstances, PackageImports #-}
module Web.Routes.Happstack where

import Control.Applicative ((<$>))
import Control.Applicative.Error (Failing(Failure, Success), ErrorMsg)
import Control.Monad (MonadPlus(mzero))
import Data.List (intercalate)
import Happstack.Server (FilterMonad(..), ServerMonad(..), WebMonad(..), ServerPartT, Response, Request(rqPaths), ToMessage(..), dirs, runServerPartT, withRequest)
import Web.Routes.Monad (RouteT(RouteT), liftRouteT, mapRouteT)
import Web.Routes.MTL
import Web.Routes.HandleT (Site, runSite)

instance (ServerMonad m) => ServerMonad (RouteT url m) where
    askRq       = liftRouteT askRq
    localRq f m = mapRouteT (localRq f) m

instance (FilterMonad a m)=> FilterMonad a (RouteT url m) where
    setFilter     = liftRouteT . setFilter
    composeFilter = liftRouteT . composeFilter
    getFilter     = mapRouteT getFilter 

instance (WebMonad a m) => WebMonad a (RouteT url m) where
    finishWith = liftRouteT . finishWith

implSite :: (Functor m, Monad m, MonadPlus m, ServerMonad m) => String -> FilePath -> Site url String (m a) -> m a
implSite domain approot siteSpec =
  do r <- implSite_ domain approot siteSpec
     case r of
       (Failure _) -> mzero
       (Success a) -> return a

implSite_ :: (Functor m, Monad m, MonadPlus m, ServerMonad m) => String -> FilePath -> Site url String (m a) -> m (Failing a)
implSite_ domain approot siteSpec =
    dirs approot $ do rq <- askRq
                      let pathInfo = intercalate "/" (rqPaths rq)
                          f        = runSite (domain ++ approot) siteSpec pathInfo
                      case f of
                        (Failure errs) -> return (Failure errs)
                        (Success sp)   -> Success <$> (localRq (const $ rq { rqPaths = [] }) sp)
{- 
implSite__ :: (Monad m) => String -> FilePath -> ([ErrorMsg] -> ServerPartT m a) -> Site url String (ServerPartT m a) -> (ServerPartT m a)
implSite__ domain approot handleError siteSpec =
    dirs approot $ do rq <- askRq
                      let pathInfo = intercalate "/" (rqPaths rq)
                          f        = runSite (domain ++ approot) siteSpec pathInfo
                      case f of
                        (Failure errs) -> handleError errs
                        (Success sp)   -> localRq (const $ rq { rqPaths = [] }) sp
-}