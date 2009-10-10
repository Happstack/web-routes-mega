{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances, UndecidableInstances #-}
module URLT.Happstack where

import Control.Applicative.Error (Failing(Failure, Success))
import Control.Monad (MonadPlus(mzero))
import Control.Monad.Trans (lift)
import Data.List (intersperse)
import Happstack.Server (FilterMonad(..), ServerMonad(..), WebMonad(..), ServerPartT, Response, Request(rqPaths), ToMessage(..), dir, runServerPartT, withRequest)
import URLT.Base (URLT(URLT), Link, mapURLT)
import URLT.HandleT (Site, runSite)

instance (ServerMonad m) => ServerMonad (URLT url m) where
    askRq       = lift askRq
    localRq f m = mapURLT (localRq f) m

instance (FilterMonad a m)=> FilterMonad a (URLT url m) where
    setFilter     = lift . setFilter
    composeFilter = lift . composeFilter
    getFilter     = mapURLT getFilter 

instance (WebMonad a m) => WebMonad a (URLT url m) where
    finishWith = lift . finishWith

-- FIXME: the prefix can only be a single directory right now
implSite :: (ToMessage a) => String -> String -> Site link Link (ServerPartT IO) a -> ServerPartT IO Response
implSite domain prefix siteSpec =
    dir (filter (/= '/') prefix) $ 
        withRequest $ \rq ->
          let link = (concat (intersperse "/" (rqPaths rq)))
          in
            do r <- runServerPartT (runSite (domain ++ prefix) siteSpec link) (rq { rqPaths = [] })
               case r of 
                 (Failure _) -> mzero
                 (Success v) -> return (toResponse v)
