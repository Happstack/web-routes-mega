{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances, UndecidableInstances #-}
module Happstack.Server.URLT 
    ( module URLT
    , implSite
    ) where
    
import Control.Monad (mzero)
import Control.Monad.Reader (mapReaderT)
import Control.Monad.Trans (lift)
import Data.List (intersperse)
import URLT 
import URLT.HandleT(Site, runSite)
import Happstack.Server (ServerMonad(askRq,localRq), FilterMonad(composeFilter, getFilter, setFilter), ServerPartT, ToMessage(..), Response, runServerPartT, Request(rqPaths), dir, withRequest)

instance (ServerMonad m) => ServerMonad (URLT url m) where
    askRq = lift askRq
    localRq f = mapReaderT (localRq f)

instance (FilterMonad a m) => FilterMonad a (URLT url m) where
    setFilter f     = lift (setFilter f)
    composeFilter f = lift (composeFilter f)
    getFilter       = mapReaderT getFilter

-- * Boilerplate code for running ourSite via Happstack
-- Easily adaptable to Network.CGI, etc.

-- FIXME: the prefix can only be a single directory right now
implSite :: (ToMessage a) => String -> String -> Site link Link (ServerPartT IO) a -> ServerPartT IO Response
implSite domain prefix siteSpec =
    dir (filter (/= '/') prefix) $ 
        withRequest $ \rq ->
          let link = (concat (intersperse "/" (rqPaths rq)))
          in
            do lift $ print link
               r <- runServerPartT (runSite (domain ++ prefix) siteSpec link) (rq { rqPaths = [] })
               case r of 
                 Nothing -> mzero
                 (Just v) -> return (toResponse v)

