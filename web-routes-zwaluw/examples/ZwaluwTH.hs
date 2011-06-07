{-# LANGUAGE TemplateHaskell, TypeOperators, ScopedTypeVariables, OverloadedStrings #-}
module Main where

import Prelude hiding (id, (.))
import Control.Category
import Control.Monad.Trans
import Text.Zwaluw.TH
import Web.Routes
import Web.Routes.Zwaluw

-- | the routes
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
    <> "users" . users
    <> rArticle . ("article" </> int . "-" . anyString)
    )
  where
    users  =  "/" . rUserOverview
           <> rUserDetail </> int

-- | Convert the 'Sitemap' into a 'Site' that can be used with web-routes
site :: Site Sitemap (IO ())
site = toSite web' sitemap
    where
      web'= \f u -> unRouteT (web u) f

-- | this function handles routing the parsed url to a handler
web :: Sitemap -> RouteT Sitemap IO ()
web url =
    case url of
      _ -> do liftIO $ print url
              s <- showURL url
              liftIO $ putStrLn s

-- | a little function to test rendering a url
showurl :: Sitemap -> String
showurl url = 
    let (ps, params) = formatPathSegments site url
    in (encodePathInfo ps params)

-- | a little function to test parsing a url
testParse :: [String] -> Either String Sitemap
testParse paths = 
    case parse1 isComplete sitemap paths of
      (Left e)  -> Left (show $ condenseErrors e)
      (Right a) -> Right a

-- | run the site using the supplied url string
test :: String -- ^ incoming url
     -> IO ()
test path = 
    case runSite "" site path of
      (Left e)   -> putStrLn e
      (Right io) -> io

-- | interactively call 'test'
main :: IO ()
main = mapM_ test =<< fmap lines getContents
        
