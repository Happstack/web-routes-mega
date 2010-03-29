module Web.Routes.Wai where

import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import Network.Wai
import Network.Wai.Enumerator
import Web.Routes.Base
import Web.Routes.PathInfo 
import Web.Routes.HandleT
import Web.Routes.Monad (RouteT, runRouteT)

handleWai_ :: (url -> String) -> (String -> Either String url) -> String -> ((url -> String) -> url -> Application) -> Application
handleWai_ fromUrl toUrl approot handler =
  \request ->
     do let fUrl = toUrl $ stripOverlap approot $ S.unpack $ pathInfo request
        case fUrl of
          (Left parseError) -> return $ Response Status404 [] $ Right $ fromLBS (L.pack parseError)
          (Right url) -> handler (showString approot . fromUrl) url request

handleWai_1 :: (url -> String) -> (String -> Either String url) -> String -> (String -> Application) -> ((url -> String) -> url -> Application) -> Application
handleWai_1 fromUrl toUrl approot handleError handler =
  \request ->
     do let fUrl = toUrl $ stripOverlap approot $ S.unpack $ pathInfo request
        case fUrl of
          (Left parseError) -> handleError parseError request
          (Right url)  -> handler (showString approot . fromUrl) url request
{-                    
handleWai_2 :: (url -> String) -> (String -> Failing url) -> String -> ((url -> String) -> url -> Application) -> (Request -> IO (Failing Response))
handleWai_2 fromUrl toUrl approot handler =
  \request ->
     do let fUrl = toUrl $ stripOverlap approot $ S.unpack $ pathInfo request
        case fUrl of
          (Failure errs) -> return (Failure errs)
          (Success url)  -> fmap Success $ handler (showString approot . fromUrl) url request          
-}

handleWai :: (PathInfo url) => String -> ((url -> String) -> url -> Application) -> Application
handleWai approot handler = handleWai_ toPathInfo fromPathInfo approot handler

handleWaiRouteT_ :: (url -> String) -> (String -> Either String url) -> String -> (url -> Request -> RouteT url IO Response) -> Application
handleWaiRouteT_  toPathInfo fromPathInfo approot handler =
   handleWai_ toPathInfo fromPathInfo approot (\toPathInfo' url request -> runRouteT (handler url request) toPathInfo') 

handleWaiRouteT :: (PathInfo url) => String -> (url -> Request -> RouteT url IO Response) -> Application
handleWaiRouteT approot handler = handleWaiRouteT_ toPathInfo fromPathInfo approot handler

waiSite :: Site url Application -> String -> Application
waiSite site approot = handleWai_ (encodePathInfo . formatPathSegments site) (withDefault site) approot (handleSite site) 
