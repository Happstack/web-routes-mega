{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, TypeFamilies, PackageImports #-}
module URLT.Transformers where

import Control.Applicative
import Control.Monad (MonadPlus)
import Control.Monad.Fix (MonadFix)
import "transformers" Control.Monad.Trans (MonadTrans(lift), MonadIO(liftIO))
import HSX.XMLGenerator (XMLGenT(..))
import URLT.Monad

instance MonadTrans (URLT url) where
  lift = liftURLT
  
instance (MonadIO m) => MonadIO (URLT url m) where  
  liftIO = lift . liftIO
