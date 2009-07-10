module HandleT where

import Control.Monad.Reader (ReaderT(runReaderT))
import URLT (URLT, Link)

data Site link url m a
    = Site { handleLink  :: link -> URLT link m a
           , defaultPage :: link
           , formatLink  :: link -> url
           , parseLink   :: url -> Maybe link 
           }

runSite :: (Monad m) => String -> Site link Link m a -> Link -> m (Maybe a)
runSite prefix site linkStr =
    let mLink = 
            case linkStr of
                 "" -> Just (defaultPage site)
                 _ -> (parseLink site) linkStr
    in
      case mLink of
        Nothing -> return Nothing
        (Just lnk) -> return . Just =<< runReaderT ((handleLink site) lnk) ((prefix ++) . (formatLink site))
