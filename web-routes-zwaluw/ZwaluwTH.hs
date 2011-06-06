{-# LANGUAGE TemplateHaskell, TypeOperators, ScopedTypeVariables #-}
import Data.Monoid
import Web.Routes
import Web.Routes.Zwaluw
import Text.Zwaluw.Core
import Text.Zwaluw.Combinators
import Text.Zwaluw.HList
import Text.Zwaluw.TH
import Prelude hiding (id, (.), (/))
import Control.Category
import Happstack.Server
import Web.Routes.Happstack

-- The router. Specifies how to parse a URL into a Sitemap and back.

type Route url = Router RouteError [String] () (url :- ())

data Sitemap
   = Home
   | UserOverview
   | UserDetail Int
   | Article Int String
   deriving (Eq, Show)

$(deriveRouters ''Sitemap)

sitemap :: Route Sitemap
sitemap =
    (  rHome
    <> lit "users" . users
    <> rArticle . (lit "article" </> int . lit "-" . string)
    )
  where
    users  =  lit "/" . rUserOverview
           <> rUserDetail </> int

toSite :: forall a r url. ((url -> [(String, String)] -> String) -> url -> a) 
       -> Router RouteError [String] () (url :- ()) 
       -> Site url a
toSite handler r@(Router pf sf) =
    Site { handleSite = handler
         , formatPathSegments =  \url ->
             case unparse1 [] r url of
               Nothing -> error "formatPathSegments failed to produce a url"
               (Just ps) -> (ps, [])
         , parsePathSegments = \paths -> 
                               let results = parse r paths
                               in
                                 case [ a | (Right (a,[])) <- results ] of
                                   ((u :- ()):_) -> Right u
                                   _             -> Left $ show $ bestErrors [ e | Left e <- results ]
                                   
{-
                               case  of
                                 (Left e) -> Left (show $ bestErrors e)
                                 (Right as) ->
                                     case [ a | (a, []) <- as ] of
                                       [] -> Left $ "No complete parses found."
                                       ((u :- ()):_) -> Right u
-}
         }

site :: Site Sitemap (IO ())
site = toSite web sitemap

web :: (Show url) => (url -> [(String, String)] -> String) -> url -> IO ()
web showFn url = 
    do print url
       putStrLn (showFn url [])

showurl :: Sitemap -> IO ()
showurl url = 
    let (ps, params) = formatPathSegments site url
    in putStrLn (encodePathInfo ps params)

test :: String -> IO ()
test path = 
    case runSite "" site path of
      (Left e) -> putStrLn e
      (Right io) -> io
