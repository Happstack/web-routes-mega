{-# LANGUAGE TemplateHaskell, TypeOperators, ScopedTypeVariables #-}
module Main where

import Prelude hiding (id, (.), (/))
import Control.Category
import Control.Monad.Trans
import Text.Zwaluw.TH
import Web.Routes
import Web.Routes.Zwaluw

-- | The routes
data Sitemap
   = Home
   | UserOverview
   | UserDetail Int
   | Article Int String
   deriving (Eq, Show)

-- derive the routing combinators like rHome, rUserOverview, etc
$(derivePrinterParsers ''Sitemap)

-- | The router. Specifies how to parse a URL into a Sitemap and back.
sitemap :: Router Sitemap
sitemap =
    (  rHome
    <> lit "users" . users
    <> rArticle . (lit "article" </> int . lit "-" . anyString)
    )
  where
    users  =  lit "/" . rUserOverview
           <> rUserDetail </> int

-- | Convert the 'Sitemap' into a 'Site' that can be used with web-routes
site :: Site Sitemap (IO ())
site = toSite web' sitemap
    where
      web'= \f u -> unRouteT (web u) f

-- | this is the  function 
web :: Sitemap -> RouteT Sitemap IO ()
web url =
    do liftIO $ print url
       s <- showURL url
       liftIO $ putStrLn s

showurl :: Sitemap -> IO ()
showurl url = 
    let (ps, params) = formatPathSegments site url
    in putStrLn (encodePathInfo ps params)

testParse :: [String] -> IO ()
testParse paths = 
    case parse1 isComplete sitemap paths of
      (Left e) -> do print e
                     putStrLn (show $ condenseErrors e)
      (Right a) -> print a

test :: String -> IO ()
test path = 
    case runSite "" site path of
      (Left e)   -> putStrLn e
      (Right io) -> io

main :: IO ()
main = mapM_ test =<< fmap lines getContents
        
