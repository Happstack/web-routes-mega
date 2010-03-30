{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, TypeFamilies, PackageImports, MultiParamTypeClasses, UndecidableInstances #-}
module Web.Routes.MTL where

import "mtl" Control.Monad.Trans (MonadTrans(lift), MonadIO(liftIO))
import Web.Routes.RouteT (RouteT(RouteT, unRouteT), liftRouteT, mapRouteT)
import Control.Monad.Reader(MonadReader(ask,local))
import Control.Monad.State(MonadState(get,put))
import Control.Monad.Writer(MonadWriter(listen, tell, pass))
import Control.Monad.Error (MonadError(throwError, catchError))
import Control.Monad.RWS (MonadRWS)
import Control.Monad.Cont(MonadCont(callCC))

instance MonadTrans (RouteT url) where
  lift = liftRouteT
  
instance (MonadIO m) => MonadIO (RouteT url m) where  
  liftIO = lift . liftIO

instance (MonadReader r m) => MonadReader r (RouteT url m) where
  ask   = liftRouteT ask
  local f = mapRouteT (local f)
  
instance (MonadState s m) => MonadState s (RouteT url m) where  
  get = liftRouteT get
  put s = liftRouteT $ put s
  
instance (MonadWriter w m) => MonadWriter w (RouteT url m) where
  tell   w = liftRouteT $ tell w
  listen m = mapRouteT listen m
  pass   m = mapRouteT pass   m
  
instance (MonadRWS r w s m) => MonadRWS r w s (RouteT url m)  
  
instance (MonadError e m) => MonadError e (RouteT url m) where
  throwError = liftRouteT . throwError
  catchError action handler = RouteT $ \f -> catchError (unRouteT action f) (\e -> unRouteT (handler e) f)
  
instance (MonadCont m) => MonadCont (RouteT url m) where
    callCC f = RouteT $ \url ->
        callCC $ \c ->
        unRouteT (f (\a -> RouteT $ \_ -> c a)) url
