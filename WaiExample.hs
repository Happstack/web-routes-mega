{-# LANGUAGE TypeFamilies #-}
module Main where

import Control.Applicative.Error
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Time (UTCTime, getCurrentTime)
import URLT.Wai
import URLT.Dispatch
import Web.Encodings (decodeUrl, encodeUrl)
import Network.Wai 
import Network.Wai.Enumerator
import Network.Wai.Handler.SimpleServer (run)
import Text.Html

-- The URL / route types

data BlogURL
  = BlogHome
  | BlogPost String
    deriving (Eq, Ord, Read, Show)
             
data SiteURL
  = MyHome
  | MyBlog BlogURL
    deriving (Eq, Ord, Read, Show)
             
-- wrapper function for generating HTML pages
             
page :: Html -> Html             
page contents = 
  header (thetitle  (toHtml "title")) +++ body contents
             
-- the BlogURL route handler Application
myBlog :: UTCTime -> (BlogURL -> String) -> BlogURL -> Application
myBlog now mkAbs BlogHome _request =
  return $ Response Status200 h $ Right c
    where
      h = [(ContentType, S.pack "text/html")]
      c = fromLBS $ L.pack $ renderHtml $ page (anchor ! [ href (mkAbs $ BlogPost "hello-world")] << (toHtml "hello-world"))
myBlog now mkAbs (BlogPost title) _request =
     return $ Response Status200 h $ Right c
      where
        h = [(ContentType, S.pack "text/plain")]
        c = fromLBS $ L.pack $ unlines
                [ mkAbs BlogHome
                , title
                , show now
                ]

-- the SiteURL route handler Application    
mySite :: UTCTime -> (SiteURL -> String) -> SiteURL -> Application
mySite _now mkAbs MyHome _request = 
  return $ Response Status200 h $ Right c
      where
        h = [(ContentType, S.pack "text/html")]
        c = fromLBS $ L.pack $ renderHtml $ page (anchor ! [ href (mkAbs $ MyBlog $ BlogPost "hello-world")] << (toHtml "hello-world"))
mySite now mkAbs (MyBlog blogURL) request =
        myBlog now (mkAbs . MyBlog) blogURL request

-- example using of using just handleWai
main1 :: IO ()
main1 =
  do now <- getCurrentTime
     let fromAbs str = maybeRead' (decodeUrl . drop 1 $ str) ("Failed to parse as url: " ++ str)
         mkAbs       = (encodeUrl . show)
     run 3000 $ handleWai mkAbs fromAbs (mySite now)

-- we can also use Dispatch

-- first we create a new type to represent the extra arguments to myBlog
data BlogArgs 
  = BlogArgs UTCTime
  deriving (Eq, Ord, Read, Show)

-- then we add a class instance that says when we apply 'dispatch' to
-- 'BlogArgs' that we want to call the myBlogD function. It also says
-- that the myBlogD function will take a BlogURL as the route argument
-- that it will return an Application. (Instead of a ServerPartT or
-- some other web monad).
instance Dispatch BlogArgs where
  type Routes BlogArgs = BlogURL
  type App BlogArgs    = Application
  dispatch             = myBlogD

-- the BlogURL route handler Application.
--
-- This is exactly the same as myBlog except that it takes BlogArgs
-- instead of UTCTime, and we have to unwrap the BlogArgs constructor.
myBlogD :: BlogArgs -> (BlogURL -> String) -> BlogURL -> Application
myBlogD (BlogArgs now) mkAbs BlogHome _request =
  return $ Response Status200 h $ Right c
    where
      h = [(ContentType, S.pack "text/html")]
      c = fromLBS $ L.pack $ renderHtml $ page (anchor ! [ href (mkAbs $ BlogPost "hello-world")] << (toHtml "hello-world"))
myBlogD (BlogArgs now) mkAbs (BlogPost title) _request =
     return $ Response Status200 h $ Right c
      where
        h = [(ContentType, S.pack "text/plain")]
        c = fromLBS $ L.pack $ unlines
                [ mkAbs BlogHome
                , title
                , show now
                ]
                
data SiteArgs = SiteArgs BlogArgs

instance Dispatch SiteArgs where
  type Routes SiteArgs = SiteURL
  type App SiteArgs    = Application
  dispatch             = mySiteD

-- the SiteURL route handler Application
--
-- this is the same as mySite except we unwrap blogArgs from SiteArgs
-- instead of passing the UTCTime value directly.
mySiteD :: SiteArgs -> (SiteURL -> String) -> SiteURL -> Application  
mySiteD _args mkAbs MyHome _request = 
  return $ Response Status200 h $ Right c
      where
        h = [(ContentType, S.pack "text/html")]
        c = fromLBS $ L.pack $ renderHtml $ page (anchor ! [ href (mkAbs $ MyBlog $ BlogPost "hello-world")] << (toHtml "hello-world"))
mySiteD (SiteArgs blogArgs) mkAbs (MyBlog blogURL) request =
        myBlogD blogArgs (mkAbs . MyBlog) blogURL request
                
-- example using of using just handleWai
-- this is the same as main1 except we replaced:
-- 
-- (mySite now)
--
-- with
--
-- (dispatch (SiteArgs (BlogArgs now)))
main2 :: IO ()
main2 =
  do now <- getCurrentTime
     let fromAbs str = maybeRead' (decodeUrl . drop 1 $ str) ("Failed to parse as url: " ++ str)
         mkAbs       = (encodeUrl . show)
     run 3000 $ handleWai mkAbs fromAbs (dispatch (SiteArgs (BlogArgs now)))
