{-# LANGUAGE TemplateHaskell, TypeOperators, ScopedTypeVariables #-}
import Data.Monoid
import Web.Routes
import Web.Routes.Zwaluw
import Text.Zwaluw.TH
import Prelude hiding (id, (.), (/))
import Control.Category
import Happstack.Server
import Web.Routes.Happstack

-- The router. Specifies how to parse a URL into a Sitemap and back.

data Sitemap
   = Home
   | UserOverview
   | UserDetail Int
   | Article Int String
   deriving (Eq, Show)

$(derivePrinterParsers ''Sitemap)

sitemap :: Router Sitemap
sitemap =
    (  rHome
    <> lit "users" . users
    <> rArticle . (lit "article" </> int . lit "-" . anyString)
    )
  where
    users  =  lit "/" . rUserOverview
           <> rUserDetail </> int

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

testParse :: [String] -> IO ()
testParse paths = 
    case parse1 isComplete sitemap paths of
      (Left e) -> do print e
                     putStrLn (show $ condenseErrors e)
      (Right a) -> print a

test :: String -> IO ()
test path = 
    case runSite "" site path of
      (Left e) -> putStrLn e
      (Right io) -> io
