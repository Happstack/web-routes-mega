{-# LANGUAGE TypeOperators #-}
module Web.Routes.Zwaluw where

import Web.Routes (Site(..))
import Text.Zwaluw.Core
import Text.Zwaluw.Error
import Text.Zwaluw.HList
import Text.Zwaluw.Strings

toSite :: ((url -> [(String, String)] -> String) -> url -> a) 
       -> Router RouteError [String] () (url :- ()) 
       -> Site url a
toSite handler r@(Router pf sf) =
    Site { handleSite = handler
         , formatPathSegments =  \url ->
             case unparse1 [] r url of
               Nothing -> error "formatPathSegments failed to produce a url"
               (Just ps) -> (ps, [])
         , parsePathSegments = mapLeft (showRouteError . condenseErrors) . (parse1 isComplete r)
         }
    where
      mapLeft f = either (Left . f) Right

