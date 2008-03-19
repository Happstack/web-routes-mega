{-# OPTIONS_GHC -fglasgow-exts #-}
module URLT where

-- Standard GHC Modules

import Control.Monad.Reader
import Data.Maybe

-- * URLT Monad Transformer

type Link = String
type URLT url = ReaderT  (url -> Link)

showURL :: (MonadReader ((->) url Link) m) => url -> m Link
showURL url =
    do showF <- ask
       return (showF url)

nestURL :: (Monad m) => (url2 -> url1) -> ReaderT (url2 -> Link) m a -> ReaderT (url1 -> Link) m a
nestURL b = withReaderT (. b)



