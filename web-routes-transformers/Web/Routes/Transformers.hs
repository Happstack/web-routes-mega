{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, TypeFamilies, PackageImports #-}
module Web.Routes.Transformers where

import "transformers" Control.Monad.Trans.Class (MonadTrans(lift))
import "transformers" Control.Monad.IO.Class (MonadIO(liftIO))
import Web.Routes.RouteT(RouteT, liftRouteT)

instance MonadTrans (RouteT url) where
  lift = liftRouteT
  
instance (MonadIO m) => MonadIO (RouteT url m) where  
  liftIO = lift . liftIO
