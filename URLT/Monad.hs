{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, TypeFamilies, PackageImports, FlexibleContexts, UndecidableInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  URLT.Monad
-- Copyright   :  (c) 2010 Jeremy Shaw
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  partners@seereason.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Declaration of the 'URLT' monad transformer
-----------------------------------------------------------------------------
module URLT.Monad where

import Control.Applicative
import Control.Monad (MonadPlus(mzero, mplus))
import Control.Monad.Fix (MonadFix(mfix))
import HSX.XMLGenerator (XMLGenT(..))

-- * URLT Monad Transformer

type Link = String

-- |monad transformer for generating URLs
newtype URLT url m a = URLT { unURLT :: (url -> Link) -> m a }
--     deriving (Functor, Monad, MonadFix, MonadPlus) -- , MonadIO, MonadTrans, MonadReader (url -> Link))

runURLT :: URLT url m a -> (url -> Link) -> m a
runURLT = unURLT

-- | Transform the computation inside a @URLT@.
mapURLT :: (m a -> n b) -> URLT url m a -> URLT url n b
mapURLT f (URLT m) = URLT $ f . m

-- | Execute a computation in a modified environment
withURLT :: ((url' -> Link) -> (url -> Link)) -> URLT url m a -> URLT url' m a
withURLT f (URLT m) = URLT $ m . f

liftURLT :: m a -> URLT url m a
liftURLT m = URLT (const m)

askURLT :: (Monad m) => URLT url m (url -> String)
askURLT = URLT return

instance (Functor m) => Functor (URLT url m) where
  fmap f = mapURLT (fmap f)
  
instance (Applicative m) => Applicative (URLT url m) where  
  pure = liftURLT . pure
  f <*> v = URLT $ \ url -> unURLT f url <*> unURLT v url

instance (Alternative m) => Alternative (URLT url m) where
    empty   = liftURLT empty
    m <|> n = URLT $ \ url -> unURLT m url <|> unURLT n url

instance (Monad m) => Monad (URLT url m) where
    return   = liftURLT . return
    m >>= k  = URLT $ \ url -> do
        a <- unURLT m url
        unURLT (k a) url
    fail msg = liftURLT (fail msg)

instance (MonadPlus m, Monad (URLT url m)) => MonadPlus (URLT url m) where
    mzero       = liftURLT mzero
    m `mplus` n = URLT $ \ url -> unURLT m url `mplus` unURLT n url

instance (MonadFix m) => MonadFix (URLT url m) where
    mfix f = URLT $ \ url -> mfix $ \ a -> unURLT (f a) url

class ShowURL m where
    type URL m
    showURL :: (URL m) -> m Link -- ^ convert a URL value into a Link (aka, a String)

instance (Monad m) => ShowURL (URLT url m) where
    type URL (URLT url m) = url
    showURL url =
        do showF <- askURLT
           return (showF url)

-- |used to embed a URLT into a larger parent url
nestURL :: (Monad m) => (url2 -> url1) -> URLT url2 m a -> URLT url1 m a
nestURL b = withURLT (. b)

crossURL :: (Monad m) => (url2 -> url1) -> URLT url1 m (url2 -> Link)
crossURL f = 
    do showF <- askURLT
       return $ \url2 -> showF (f url2)
