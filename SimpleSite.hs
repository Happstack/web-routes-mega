{-# OPTIONS_GHC -W -fglasgow-exts #-}
module Main where

-- Standard GHC Modules

import Control.Monad.Reader
import Data.Maybe
import Data.Word
import Network.URI


-- Third Party Modules

import Network.CGI hiding (Html)
import Text.XHtml


-- * URLT Monad Transformer

{-

This is a really simple, but effective way of handling URLs. It provides:

 (1) compile time assurance that you have no broken links (note: must
     compile with -W to see the warnings).

 (2) Collision free namespace for modules -- even if a module is used
     by several different modules.

All we do is use the ReaderT monad transformer to hold a function that
can type a type 'url' into a hyper-link.

For a photo module we might create a 'url' type like:

data Photos
    = RecentPhotos
    | PhotoView Word32
      deriving (Read, Show)

For a Blog module we might create a 'url' type like:

data Blog
    = RecentEntries
    | BlogView Word32
    | BlogPhotos Photos
      deriving (Read, Show)

Note that it uses the 'Photos' module.

We might use both those modules in our site with at url like:

data SimpleSite
    = HomePage
    | MyBlog Blog
    | PhotoGallery Photos
      deriving (Read, Show)

Note that SimpleSite also uses the 'Photos' module -- but it can be a
different photo gallery than what the Blog module uses, because they
have different URLs.

PhotoGallery -> Photos vs. MyBlog -> BlogPhotos -> Photos

I believe this solution can be fitted to a large number of different
systems, like WASH, Hope, HAppS, etc.

-}

type Link = String
type URLT url = ReaderT  (url -> Link)

-- |This function is used when we want to turn a url into a string
-- that we can use as an argument to href.
showURL :: (MonadReader ((->) url Link) m) => url -> m Link
showURL url =
    do showF <- ask
       return (showF url)

-- |This function is used to nest one module under another.
nestURL :: (Monad m) => (url2 -> url1) -> ReaderT (url2 -> Link) m a -> ReaderT (url1 -> Link) m a
nestURL b = withReaderT (. b)

-- * Example Usage

-- Here is an example that shows how you might actually use the above
-- functions.

-- |Function to handle incoming requests
-- 'scriptName' is the name of this script
-- 'site' is the function that will handle the incoming request
-- 'defaultUrl' is the defaultUrl to invoke is none is explicitly requested
-- 'queryStr' in the QUERY_STRING that contains the request
--
-- This is where the 'url' gets turned into a string and back. You can
-- do it however you want. I do it a pretty brain-dead way for this example.
handleURL :: (Read url, Show url) => String -> (url -> URLT url m Html) -> url -> String -> m Html
handleURL scriptName site defaultUrl queryStr =
    let req =
            case reads (decode queryStr) of
              [(url,"")] -> url
              _ -> defaultUrl
    in
      runReaderT (site req) (((scriptName++"?")++) . encode . show)
    where encode = escapeURIString isUnreserved
          decode = unEscapeString

-- |This function gets the CGI variable, invokes handleURL, and
-- displays the final Html. It is not very exciting.
runSite :: (Read url, Show url) => (url -> URLT url (CGIT IO) Html) -> url -> IO ()
runSite site defaultUrl =
    runCGI $ do queryStr <- liftM (fromMaybe "")  $ getVar "QUERY_STRING"
                scriptName <- liftM fromJust $ getVar "SCRIPT_NAME"
                html <- handleURL scriptName site defaultUrl queryStr
                output (renderHtml html)

main :: IO ()
main = runSite simpleSite HomePage

-- * Photos Module

-- A simple module for displaying photos. Would eventually have more
-- photo gallery options.

-- |The Photos module has two features:
-- (1) Return a list of recent photos
-- (2) Show a specific photo
data Photos
    = RecentPhotos
    | PhotoView Word32
      deriving (Read, Show)

-- |PhotoConfig - configuration for a photo gallery. Currently just a
-- title, but ultimately, it would include a reference to a database
-- of actual images.
data PhotoConfig
    = PhotoConfig { photoGalleryTitle :: String }

photos :: (Monad m) => PhotoConfig -> Photos -> URLT Photos m Html
-- If we get a request for recent photos, we return a list of recent
-- photos. Currently hardcoded to just 2 photos.
photos _ RecentPhotos =
    do photo1Url <- showURL (PhotoView 1)
       photo2Url <- showURL (PhotoView 2)
       return $ (((anchor ! [href photo1Url]) (toHtml "photo 1")) +++ br +++ 
                 ((anchor ! [href photo2Url]) (toHtml "photo 2")))
-- View a specific photo. Currently, you have to use your imagination.
photos config (PhotoView w) =
    do return $ h1 (toHtml (photoGalleryTitle config)) +++ ("You are now looking at photo " ++ show w)

-- * Blog Module

-- |The Blog has three features:
-- (1) get a list of the most recent blog entries
-- (2) show a specific blog entry
-- (3) show a photo associated with a specific blog entry (note: uses the Photos module)
data Blog
    = RecentEntries
    | BlogView Word32
    | BlogPhotos Photos
      deriving (Read, Show)

blogPhotoConfig = PhotoConfig { photoGalleryTitle = "Blog Photos" }

blog :: (Monad m) => Blog -> URLT Blog m Html
blog RecentEntries =
    do entry1Url <- showURL (BlogView 1)
       entry2Url <- showURL (BlogView 2)
       return $ ((anchor ! [href entry1Url]) (toHtml "blog entry 1")) +++ br +++ ((anchor ! [href entry2Url]) (toHtml "blog entry 2"))
blog (BlogView w) =
    do photoUrl <- nestURL BlogPhotos $ showURL (PhotoView 1)
       return $ toHtml ("This is blog entry " ++ show w) +++ br +++ 
                (anchor ! [href photoUrl]) << toHtml "a picture related to this blog entry."
blog (BlogPhotos p) =
    do body <- nestURL BlogPhotos $ photos blogPhotoConfig p
       return $ defPage body


-- * Top-level site module

-- | SimpleSite
-- This site has three sections:
-- (1) default home page
-- (2) a blog
-- (3) a photo gallery

-- Note that this photogallery is *different* from the photos
-- associated with the blogs. Even though they use the same Photos
-- module, they pass in different PhotoConfigs

data SimpleSite
    = HomePage
    | MyBlog Blog
    | PhotoGallery Photos
      deriving (Read, Show)

mainPhotoGallery = PhotoConfig { photoGalleryTitle = "Main Photo Gallery" }

simpleSite :: (Monad m) => SimpleSite -> URLT SimpleSite m Html
-- Default homepage
simpleSite HomePage =
    do blogs <- nestURL MyBlog $ blog RecentEntries
       photos <- nestURL PhotoGallery $ photos mainPhotoGallery RecentPhotos
       return $ defPage (blogs +++ br +++ photos)
-- Process Blog request - note how it adds extra stuff to the bottom of the blog
simpleSite (MyBlog b) =
    do body <- nestURL MyBlog $ blog b
       return $ defPage (body +++
                         p << toHtml "This could be adsense, or something else added by the top-level SimpleSite -- that the Blog module knows nothing about.")

-- Process Photos request
simpleSite (PhotoGallery p) =
    do body <- nestURL PhotoGallery $ photos mainPhotoGallery p
       return $ defPage body

-- default page layout function
defPage :: Html -> Html
defPage thebody =
    ((header 
      (thetitle (toHtml "Simple Site")) +++
      (thelink ! [href "./simplesite.css", rel "stylesheet", thetype "text/css"] << noHtml)) +++
     (body thebody))
