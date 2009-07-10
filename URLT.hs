module URLT where

import Control.Monad.Reader (MonadReader(ask), ReaderT, withReaderT)
import URLT

-- * URLT Monad Transformer

type Link = String
type URLT url = ReaderT  (url -> Link)

showURL :: (Monad m) => url -> URLT url m Link
showURL url =
    do showF <- ask
       return (showF url)

nestURL :: (Monad m) => (url2 -> url1) -> URLT url2 m a -> URLT url1 m a
nestURL b = withReaderT (. b)

crossURL :: (Monad m) => (url2 -> url1) -> URLT url1 m (url2 -> Link)
crossURL f = 
    do showF <- ask
       return $ \url2 -> showF (f url2)
