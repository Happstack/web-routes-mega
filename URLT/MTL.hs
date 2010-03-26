{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, TypeFamilies, PackageImports #-}
module URLT.MTL where

import "mtl" Control.Monad.Trans (MonadTrans(lift), MonadIO(liftIO))
import URLT.Monad (URLT, liftURLT)

instance MonadTrans (URLT url) where
  lift = liftURLT
  
instance (MonadIO m) => MonadIO (URLT url m) where  
  liftIO = lift . liftIO
