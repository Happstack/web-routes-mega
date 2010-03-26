{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, TypeFamilies, PackageImports #-}
module URLT.Transformers where

import "transformers" Control.Monad.Trans (MonadTrans(lift), MonadIO(liftIO))
import URLT.Monad(URLT, liftURLT)

instance MonadTrans (URLT url) where
  lift = liftURLT
  
instance (MonadIO m) => MonadIO (URLT url m) where  
  liftIO = lift . liftIO
