{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, TypeFamilies #-}
module URLT.Base where

import Control.Applicative
import Control.Monad (MonadPlus)
import Control.Monad.Trans (MonadTrans, MonadIO)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Reader (MonadReader(ask), ReaderT(ReaderT), mapReaderT, withReaderT)
import HSX.XMLGenerator (XMLGenT(..))

-- * URLT Monad Transformer

type Link = String

-- |monad transformer for generating URLs
newtype URLT url m a = URLT { unURLT :: ReaderT  (url -> Link) m a }
    deriving (Functor, Monad, MonadFix, MonadPlus, MonadIO, MonadTrans, MonadReader (url -> Link))

-- NOTE: the Monad m requirement comes from the Functor ReaderT instance
instance (Applicative m, Monad m) => Applicative (URLT url m) where
    pure = return
    (URLT (ReaderT f)) <*> (URLT (ReaderT a))
        = URLT $ ReaderT $ \env -> (f env) <*> (a env)

class ShowURL m where
    type URL m
    showURL :: (URL m) -> m Link -- ^ convert a URL value into a Link (aka, a String)

instance (Monad m) => ShowURL (URLT url m) where
    type URL (URLT url m) = url
    showURL url =
        do showF <- ask
           return (showF url)

-- |similar to withReaderT
withURLT :: ((url' -> Link) -> (url -> Link)) -> URLT url m a -> URLT url' m a
withURLT f (URLT r) = URLT $ withReaderT f r

mapURLT :: (m a -> n b) -> URLT url m a -> URLT url n b
mapURLT f (URLT r) = URLT $ mapReaderT f r

-- |used to embed a URLT into a larger parent url
nestURL :: (Monad m) => (url2 -> url1) -> URLT url2 m a -> URLT url1 m a
nestURL b = withURLT (. b)

crossURL :: (Monad m) => (url2 -> url1) -> URLT url1 m (url2 -> Link)
crossURL f = 
    do showF <- ask
       return $ \url2 -> showF (f url2)

