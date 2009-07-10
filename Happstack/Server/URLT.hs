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
import HandleT(Site, runSite)
import Happstack.Server (ServerMonad(askRq,localRq), FilterMonad(composeFilter, getFilter, setFilter), ServerPartT, ToMessage(..), Response, runServerPartT, Request(rqPaths), withRequest)

instance (ServerMonad m) => ServerMonad (URLT url m) where
    askRq = lift askRq
    localRq f = mapReaderT (localRq f)

instance (FilterMonad a m) => FilterMonad a (URLT url m) where
    setFilter f     = lift (setFilter f)
    composeFilter f = lift (composeFilter f)
    getFilter       = mapReaderT getFilter

-- * Boilerplate code for running ourSite via Happstack
-- Easily adaptable to Network.CGI, etc.

implSite :: (ToMessage a) => String -> Site link Link (ServerPartT IO) a -> ServerPartT IO Response
implSite prefix siteSpec = 
    withRequest $ \rq ->
          let link = (concat (intersperse "/" (rqPaths rq)))
          in
            do lift $ print link
               r <- runServerPartT (runSite prefix siteSpec link) (rq { rqPaths = [] })
               case r of 
                 Nothing -> mzero
                 (Just v) -> return (toResponse v)

