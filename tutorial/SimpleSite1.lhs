> {-# LANGUAGE DeriveDataTypeable, FlexibleContexts #-}
> module Main where


> import Control.Concurrent
> import Control.Monad.Trans
> import HAppS.Server hiding (method, dir)
> import Text.XHtml
> import Network.URI

<h1>404 No More!</h1> 
 <p>This post shows a simple way to use the Reader Monad Transformer to:</p>
    <ol>
     <li>Ensure that all the internal links in your web app are valid.</li>
     <li>Ensure that when a component is used multiple times, each instance gets a unique set of URLs.</li>
    </ol>
      

 <h2>An Obvious Start</h2>
    <p>The first step is to present the links as data types in the
    code. For example, let's imagine we have a simple image
    gallery. The gallery has two views:</p>
      <ol>
	<li>View a thumbnail gallery of all the images</li>
	<li>View an individual image</li>
      </ol>
    <p>Furthermore, when viewing an invidual image, we might view it:</p>
    <ol>
      <li>Fullsize</li>
      <li>Scaled down to screen size</li>
    </ol>
    We can represent navigating to those pages with the following types:

> data Gallery
>    = Thumbnails 
>    | ShowImage Int Size
>    deriving (Read, Show)

> data Size
>    = Full
>    | Screen
>    deriving (Read, Show)

<p>Note that the data type only specifies which page to view. It does not
include all of the information about the page, such as the filepath to
the image, the image size etc. This data type only includes the type
of information normally found in a URL.</p>

<p>Next we need some functions to turn our type into a
<code>Link</code> and back. Here we will make <code>Link</code> be a
simple <code>String</code>, but it could be something fancier, such as
<code>Network.URI</code> if desired.</p>

<p>For the first pass, we will just use <code>show</code> and
<code>read</code> with some extra functions to encode and decode
special characters which can not appear in a URI. Later we will see
how to implement <code>showLink</code> and <code>readLink</code> so
that they generate prettier and more user friendly links.</p>

> type Link = String

> showLink :: (Show a) => a -> Link
> showLink = escapeURIString isUnescapedInURI . show 

> readLink :: (Read a) => Link -> Maybe a
> readLink = readM . unEscapeString
>     where
>       readM :: (Read a) => String -> Maybe a
>       readM str =
>           case reads str of
>             [(a,"")] -> Just a
>             o -> Nothing

<p>Next we implement a function which will interpret the
<code>Gallery</code> link and display the corresponding page:</p>

> -- dummy implementation for didactic purposes
> gallery :: String -> Gallery -> Html
> gallery username Thumbnails = 
>     let img1 = showLink (ShowImage 1 Full)
>     in pageTemplate 
>            ((toHtml $ "Showing " ++ username ++ "'s gallery thumbnails.") +++ 
>             br +++
>             (anchor (toHtml "image 1") ! [href img1]))
> gallery username (ShowImage i s) = 
>     pageTemplate (toHtml $ "showing " ++ username ++ "'s image number " ++ 
>                   show i ++ " at " ++ show s ++ " size.")

> pageTemplate :: Html -> Html
> pageTemplate thebody =
>     ((header 
>       (thetitle (toHtml "Simple Site"))) +++
>      (body thebody))

<h2>Conclusion</h2>
<h3>The Good</h3>
<p>There are two good things to note about <code>gallery</code>.</p>
<ol> 
 <li>Looking up the page associated with the incoming link uses
standard Haskell pattern matching. Therefore, the compiler can warn
use if we have incomplete pattern matches.</li>
 <li>In the code, the links are represented as datatypes, not
 <code>String</code>s. This means the compiler will catch typos,
 mismatched types, missing arguments, and other similar errors at compile time.
 </li>
</ol>

<h3>The Bad</h3>

We have eliminated some causes of invalid links, but there are still
many uncaught errors lurking around. For example, if we replace,
<code>showLink (ShowImage 1 Full)</code> with <code>showLink
True</code>, the code will still compile without any errors. But, at
runtime, when you clicked on the link, it will try to find a match for
<code>True</code> and get a 404.

Additionally, if we try to use the <code>Gallery</code> library in two
places in a larger site, the generated URIs will be wrong and
non-unique. For example, imagine if we had a site like:

> data OurSite 
>     = HomePage
>     | MyGallery Gallery
>     | YourGallery Gallery
>       deriving (Read, Show)

In the next post we will see how to address these two issues.

<h2>The Rest Of This Example</h2>

The remaining code is just some boilerplate code for calling our
<code>gallery</code> example. It uses HAppS, but could easily be
adapted to use <code>Network.CGI</code>.

> data Site link a
>     = Site { handleLink :: link -> a
>            , defaultPage :: link
>            }

> runSite :: (Read link) => Site link a -> Link -> Maybe a
> runSite site linkStr =
>     let mLink = 
>             case linkStr of
>                  "" -> Just (defaultPage site)
>                  _ -> readLink linkStr
>     in
>       fmap (handleLink site) mLink

> simpleSite :: Site Gallery Html
> simpleSite =
>     Site { handleLink = gallery "Jeremy"
>          , defaultPage = Thumbnails
>          }

> -- * Boilerplate code for running simpleSite via HAppS. 
> -- Easily adaptable to Network.CGI, etc.

> implURL :: [ServerPartT IO Response]
> implURL =
>     [ withRequest $ \rq ->
>           let link = (concat (take 1 (rqPaths rq)))
>           in
>             do lift $ print link
>                return . toResponse $ runSite simpleSite link
>     ]

> main :: IO ()
> main = 
>     do tid <- forkIO $ simpleHTTP nullConf implURL
>        putStrLn "running..."
>        waitForTermination
>        killThread tid
