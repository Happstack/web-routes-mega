{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances, UndecidableInstances, PackageImports #-}
module URLT.Happstack where

import Control.Applicative.Error (Failing(Failure, Success))
import Control.Monad (MonadPlus(mzero))
import Data.List (intersperse)
import Happstack.Server (FilterMonad(..), ServerMonad(..), WebMonad(..), ServerPartT, Response, Request(rqPaths), ToMessage(..), dir, runServerPartT, withRequest)
import URLT.Base (URLT(URLT), Link, liftURLT, mapURLT)
import URLT.MTL
import URLT.HandleT (Site, runSite)

instance (ServerMonad m) => ServerMonad (URLT url m) where
    askRq       = liftURLT askRq
    localRq f m = mapURLT (localRq f) m

instance (FilterMonad a m)=> FilterMonad a (URLT url m) where
    setFilter     = liftURLT . setFilter
    composeFilter = liftURLT . composeFilter
    getFilter     = mapURLT getFilter 

instance (WebMonad a m) => WebMonad a (URLT url m) where
    finishWith = liftURLT . finishWith

-- FIXME: the prefix can only be a single directory right now
implSite :: (Monad m) => String -> String -> Site link Link (ServerPartT m) a -> ServerPartT m a
implSite domain prefix siteSpec =
    dir (filter (/= '/') prefix) $ 
        withRequest $ \rq ->
          let link = (concat (intersperse "/" (rqPaths rq)))
          in
            do r <- runServerPartT (runSite (domain ++ prefix) siteSpec link) (rq { rqPaths = [] })
               case r of 
                 (Failure _) -> mzero
                 (Success v) -> return v

