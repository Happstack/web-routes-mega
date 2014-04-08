module Web.Routes.Wai where

import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Text           (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Network.Wai         ( Application, Request, Response, rawPathInfo
                           , responseLBS)
import Network.HTTP.Types  (status404)
import Web.Routes.Base     (decodePathInfo, encodePathInfo)
import Web.Routes.PathInfo (PathInfo(..), fromPathInfo, stripOverlapBS
                           , toPathInfoParams)
import Web.Routes.RouteT   (RouteT, unRouteT)
import Web.Routes.Site     (Site(..))

-- | a low-level function for convert a parser, printer, and routing function into an 'Application'
handleWaiError :: (url -> [(Text, Maybe Text)] -> Text) -- ^ function to convert a 'url' + params into path info + query string
               -> (S.ByteString -> Either String url)   -- ^ function to parse path info into 'url'
               -> S.ByteString                          -- ^ app root
               -> (String -> Application)               -- ^ function to call if there is a decoding error, argument is the parse error
               -> ((url -> [(Text, Maybe Text)] -> Text) -> url -> Application)  -- ^ routing function
               -> Application
handleWaiError fromUrl toUrl approot handleError handler =
  \request ->
     do let fUrl = toUrl $ stripOverlapBS approot $ rawPathInfo request
        case fUrl of
          (Left parseError) -> handleError parseError request
          (Right url)  -> handler (\url params -> (Text.decodeUtf8 approot) `Text.append` (fromUrl url params)) url request

-- | a low-level function for convert a parser, printer, and routing function into an 'Application'
--
-- returns 404 if the url parse fails.
handleWai_ :: (url -> [(Text, Maybe Text)] -> Text) -- ^ function to convert a 'url' + params into path info + query string
           -> (S.ByteString -> Either String url)         -- ^ function to parse path info into 'url'
           -> S.ByteString                                -- ^ app root
           -> ((url -> [(Text, Maybe Text)] -> Text) -> url -> Application) -- ^ routing function
           -> Application
handleWai_ fromUrl toUrl approot handler =
    handleWaiError fromUrl toUrl approot handleError handler
    where
      handleError :: String -> Application
      handleError parseError = \_request -> return $ responseLBS status404 [] (L.pack parseError)

-- | function to convert a routing function into an Application by
-- leveraging 'PathInfo' to do the url conversion
handleWai :: (PathInfo url) =>
             S.ByteString -- ^ approot
          -> ((url -> [(Text, Maybe Text)] -> Text) -> url -> Application) -- ^ routing function
          -> Application
handleWai approot handler = handleWai_ toPathInfoParams fromPathInfo approot handler

-- | a function to convert a parser, printer and routing function into an 'Application'.
--
-- This is similar to 'handleWai_' expect that it expects the routing function to use 'RouteT'.
handleWaiRouteT_ :: (url -> [(Text, Maybe Text)] -> Text) -- ^ function to convert a 'url' + params into path info + query string
                 -> (S.ByteString -> Either String url)         -- ^ function to parse path info into 'url'
                 -> S.ByteString                                -- ^ app root
                 -> (url -> Request -> RouteT url IO Response) -- ^ routing function
                 -> Application
handleWaiRouteT_  toPathInfo fromPathInfo approot handler =
   handleWai_ toPathInfo fromPathInfo approot (\toPathInfo' url request -> unRouteT (handler url request) toPathInfo')

-- | convert a 'RouteT' based routing function into an 'Application' using 'PathInfo' to do the url conversion
handleWaiRouteT :: (PathInfo url) =>
                   S.ByteString  -- ^ app root
                -> (url -> Request -> RouteT url IO Response) -- ^ routing function
                -> Application
handleWaiRouteT approot handler = handleWaiRouteT_ toPathInfoParams fromPathInfo approot handler

-- |convert a 'Site url Application' into a plain-old 'Application'
waiSite :: Site url Application -- ^ Site
        -> S.ByteString               -- ^ approot, e.g. http://www.example.org/app
        -> Application
waiSite site approot = handleWai_ formatURL (parsePathSegments site . decodePathInfo) approot (handleSite site)
    where
      formatURL url params =
          let (paths, moreParams) = formatPathSegments site url
          in encodePathInfo paths (params ++ moreParams)
