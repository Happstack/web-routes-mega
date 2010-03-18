{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, TypeFamilies, PackageImports #-}
module URLT.MTL where

import Control.Applicative
import Control.Monad (MonadPlus)
import Control.Monad.Fix (MonadFix)
import "mtl" Control.Monad.Trans (MonadTrans(lift), MonadIO(liftIO))
import HSX.XMLGenerator (XMLGenT(..))
import URLT.Base

instance MonadTrans (URLT url) where
  lift = liftURLT
  
instance (MonadIO m) => MonadIO (URLT url m) where  
  liftIO = lift . liftIO
