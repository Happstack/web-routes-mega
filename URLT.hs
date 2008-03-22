{-# LANGUAGE FlexibleContexts #-}
module URLT where

-- Standard GHC Modules

import Control.Monad.Reader
import Data.Maybe

-- * URLT Monad Transformer

type Link = String
type URLT url = ReaderT  (url -> Link)

showURL :: (Monad m) => url -> URLT url m Link
showURL url =
    do showF <- ask
       return (showF url)

nestURL :: (Monad m) => (url2 -> url1) -> URLT url2 m a -> URLT url1 m a
nestURL b = withReaderT (. b)
