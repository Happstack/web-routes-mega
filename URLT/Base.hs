{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module URLT.Base where

import Control.Monad (MonadPlus)
import Control.Monad.Trans (MonadTrans, MonadIO)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Reader (MonadReader(ask), ReaderT, mapReaderT, withReaderT)

-- * URLT Monad Transformer

type Link = String

-- |monad transformer for generating URLs
newtype URLT url m a = URLT { unURLT :: ReaderT  (url -> Link) m a }
    deriving (Functor, Monad, MonadFix, MonadPlus, MonadIO, MonadTrans, MonadReader (url -> Link))

-- |similar to withReaderT
withURLT :: ((url' -> Link) -> (url -> Link)) -> URLT url m a -> URLT url' m a
withURLT f (URLT r) = URLT $ withReaderT f r

mapURLT :: (m a -> n b) -> URLT url m a -> URLT url n b
mapURLT f (URLT r) = URLT $ mapReaderT f r

-- |convert a URL value into a Link (aka, a String)
showURL :: (Monad m) => url -> URLT url m Link
showURL url =
    do showF <- ask
       return (showF url)

-- |used to embed a URLT into a larger parent url
nestURL :: (Monad m) => (url2 -> url1) -> URLT url2 m a -> URLT url1 m a
nestURL b = withURLT (. b)

crossURL :: (Monad m) => (url2 -> url1) -> URLT url1 m (url2 -> Link)
crossURL f = 
    do showF <- ask
       return $ \url2 -> showF (f url2)
