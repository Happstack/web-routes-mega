module URLT.HandleT where

import Control.Applicative.Error(Failing(Failure, Success))
import Control.Monad.Reader (ReaderT(runReaderT))
import URLT.Base (URLT, Link, runURLT)

data Site link url m a
    = Site { handleLink  :: link -> URLT link m a
           , defaultPage :: link
           , formatLink  :: link -> url
           , parseLink   :: url -> Failing link 
           }

instance (Functor m) => Functor (Site u l m) where
    fmap f site =
        site { handleLink = \link -> fmap f ((handleLink site) link) }

runSite :: (Monad m) => String -> Site link Link m a -> Link -> m (Failing a)
runSite prefix site linkStr =
    let fLink = 
            case linkStr of
                 "" -> Success (defaultPage site)
                 _ -> (parseLink site) linkStr
    in
      case fLink of
        (Failure errs) -> return (Failure errs)
        (Success lnk) -> return . Success =<< runURLT ((handleLink site) lnk) ((prefix ++) . (formatLink site))
