<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html>
<head>
<title>404 No More!, Part II</title>
<link type='text/css' rel='stylesheet' href='hscolour.css' />
</head>
<body>
First some header stuff.

> {-# LANGUAGE DeriveDataTypeable, FlexibleContexts #-}
> module Main where


> import Control.Concurrent
> import Control.Monad.Trans
> import Control.Monad.Reader
> import HAppS.Server hiding (method, dir)
> import Text.XHtml
> import Network.URI

<h1>404 No More!, Part II</h1>
 (<i><a href='./SimpleSite1.html'>Part I</a> and <a href='./SimpleSite3.html'>Part III</a></i>)
 <p>In the previous part, we ended with the following types:</p>


> data OurSite 
>     = HomePage
>     | MyGallery Gallery
>     | YourGallery Gallery
>       deriving (Read, Show)

> data Gallery
>    = Thumbnails 
>    | ShowImage Int Size
>    deriving (Read, Show)

> data Size
>    = Full
>    | Screen
>    deriving (Read, Show)

<p>The problems we faced were:</p>
 <ol>
  <li>How to ensure that <code>showLink</code> was only called using
 data-types that were actually handled by the site</li>
 <li>How to reuse the <code>gallery</code> module in a larger site</li>
 <li>How to ensure multiple instances of <code>gallery</code> don't
 collide.</li>
 </ol>

<h2>The Solution</h2>

 <p>To solve the first problem, we need to restrict the types that
 <code>showLink</code> can be applied to. To solve the second two
 problems, we need some way to record the current context so that we
 can use it when we generate the <code>Link</code>.</p>

 <p>The perfect tool for this job is the Reader monad (also known as
 the Enviroment monad):</p>

> type Link = String
> type LinkM link a = Reader (link -> Link) a

 <p>We store a function in the enviroment which can be used to turn a
 data type into a <code>Link</code>. The <code>showLink</code>
 function is now parameterized over this monad as follows:
 </p>

> showLink :: link -> LinkM link Link
> showLink url =
>    do showF <- ask
>       return (showF url)

 <p>Now <code>showLink</code> can only be called on types supported by the
 current environment. As a practical example, our <code>gallery</code>
 function will now looks like this:
 </p>

> -- dummy implementation for didactic purposes
> gallery :: String -> Gallery -> LinkM Gallery Html
> gallery username Thumbnails = 
>     do img1 <- showLink (ShowImage 1 Full)
>        return $ pageTemplate 
>            ((toHtml $ "Showing " ++ username ++ "'s gallery thumbnails.") +++ 
>             br +++
>             (anchor (toHtml "image 1") ! [href img1]))
> gallery username (ShowImage i s) = 
>     return $ pageTemplate (toHtml $ "showing " ++ username ++ "'s image number " ++ 
>                            show i ++ " at " ++ show s ++ " size.")

> pageTemplate :: Html -> Html
> pageTemplate thebody =
>     ((header 
>       (thetitle (toHtml "Simple Site"))) +++
>      (body thebody))

 <p>
 So, now if we tried to call <code>showLink True</code> inside gallery
 we would get an error like:
 </p>
 <pre>/home/stepcut/n-heptane/projects/haskell/urlt/tutorial/SimpleSite2.lhs:81:26:
    Couldn't match expected type `Gallery' against inferred type `Bool'
    In the first argument of `showLink', namely `True'
    In a 'do' expression: img1 <- showLink True
    In the expression:
        do img1 <- showLink True
             return
           $ pageTemplate
               ((toHtml $ "Showing " ++ username ++ "'s gallery thumbnails.")
              +++ br +++ (anchor (toHtml "image 1") ! [href img1]))
Failed, modules loaded: none.</pre>

 <p>The LinkM monad also provides us with a convenient way to embed
 the <code>gallery</code> library into a larger site:
 </p> 

> nestLink :: (url2 -> url1) -> LinkM url2 a -> LinkM url1 a
> nestLink b = withReader (. b)

 <p>Here is how we would use <code>nestLink</code> to support <code>OurSite</code></p>

> ourSite :: OurSite -> LinkM OurSite Html
> ourSite HomePage =
>     do myGallery <- showLink $ MyGallery Thumbnails
>        yourGallery <- nestLink YourGallery $ showLink Thumbnails
>        return $ pageTemplate (toHtml "go to " +++ br +++
>                          (anchor (toHtml "my gallery")) ! [href myGallery ]  +++ br +++
>                          (anchor (toHtml "your gallery")) ! [href yourGallery ] 
>                         )
> ourSite (MyGallery g) =
>     nestLink MyGallery $ gallery "Jeremy Shaw" g
> ourSite (YourGallery g) =
>     nestLink YourGallery $ gallery "someone else" g

 <p> Note that in the alternative for <code>HomePage</code> we create
 the links to the galleries two different ways. But in both instances
 the type of the link is <code>OurSite</code>. </p>

 <p>More interesting is the alternatives which match on
 <code>MyGallery</code> and <code>YourGallery</code>. To nest the
 <code>gallery</code> we just use <code>nestLink MyGallery</code> and
 <code>nestLink YourGallery</code>. The <code>gallery</code> library
 itself needs no additional adjustments.</p>

 <p>Additionally, let's say we forgegt the nestLink
 in the last alternative and instead write:</p>

 <pre>ourSite (YourGallery g) =
     gallery "someone else" g</pre>

 <p>At compile time we will get an error like:</p>

 <pre>/home/stepcut/n-heptane/projects/haskell/urlt/tutorial/SimpleSite2.lhs:134:6:
    Couldn't match expected type `OurSite'
           against inferred type `Gallery'
      Expected type: Reader (OurSite -> Link) Html
      Inferred type: LinkM Gallery Html
    In the expression: gallery "someone else" g
    In the definition of `ourSite':
        ourSite (YourGallery g) = gallery "someone else" g
Failed, modules loaded: none.</pre>

<h2>Conclusion</h2>

 <h3>The Good</h3>

 <p>With the addition of the LinkM monad, we have neatly addressed the
 goals set forth. We have leveraged the type-checker to ensure that
 all the internal links are associated with some code that handles
 them. Additionally, we can import third-party modules into a larger
 site, and the generate links are automatically adjusted to match the
 site structure. We can use the same 3rd-party module in more than one
 place and the links will not collide.
 </p>

 <h3>The Bad</h3>
 <p>The implementation presented in this part is a bit limiting,
 because we can not use the IO monad or other similar Monads our site
 function. In the next part we will make some trivial modifications to
 use the Reader Monad Transformer instead of the plain Reader Monad.</p>

 <p>The <code>Link</code>s generated by this code are very scary
 looking. In the next part, we would show how to make prettier looking
 links.</p>

 <h3>Other Notes</h3>

 <p>The type-safety of the Links is in part due to the
 fact that the argument to the handler function is the same as the
 type of link the monad is parameterized over. For example:</p>
 
 <pre>ourSite :: OurSite -> LinkM OurSite Html</pre>

 <p>One way to enforce this required is to code it into the type which
 represents our site:</p>

>
> data Site link a
>     = Site { handleLink  :: link -> LinkM link a
>            , defaultPage :: link
>            , formatLink  :: link -> Link
>            , parseLink   :: Link -> Maybe link 
>            }

> ourSiteSpec :: Site OurSite Html
> ourSiteSpec =
>     Site { handleLink = ourSite
>          , defaultPage = HomePage
>          , formatLink = escapeURIString isUnescapedInURI . show
>          , parseLink = readLink
>          }


<p>Alternatively, we might choose to encode it in the LinkM type directly:</p>

> type LinkM' link a = link -> Reader (link -> Link) a

<p>However, I find that confusing. Consider:</p>

> testFunc :: String -> LinkM' link link
> testFunc str lnk = return lnk

 <p>The type appears to indicate that testFunc takes one argument instead of two.</p>

 <p>Also, we can not write the type signature for nestLink using the
 <code>LinkM'</code> type synonym. For these reasons I prefer the
 first option.</p>

 <h3>Next</h3>

 <p>In <a href="./SimpleSite3.html">part III</a>, we will make some small, final adjustments to
 make the library more usuable for real world projects</p>

<h2>Remaining Boilerplate Code</h2>

The remaining code just wraps the example up into a working example.

> runSite :: (Show link, Read link) => Site link a -> Link -> Maybe a
> runSite site linkStr =
>     let mLink = 
>             case linkStr of
>                  "" -> Just (defaultPage site)
>                  _ -> (parseLink site) linkStr
>     in
>       case mLink of
>         Nothing -> Nothing
>         (Just lnk) -> Just $ runReader ((handleLink site) lnk) (formatLink site)
>     where

> readLink :: (Read a) => Link -> Maybe a
> readLink = readM . unEscapeString
>     where
>     readM :: (Read a) => String -> Maybe a
>     readM str =
>           case reads str of
>             [(a,"")] -> Just a
>             o -> Nothing

> -- * Boilerplate code for running ourSite via HAppS. 
> -- Easily adaptable to Network.CGI, etc.

> implURL :: [ServerPartT IO Response]
> implURL =
>     [ withRequest $ \rq ->
>           let link = (concat (take 1 (rqPaths rq)))
>           in
>             do lift $ print link
>                return . toResponse $ runSite ourSiteSpec link
>     ]

> main :: IO ()
> main = 
>     do tid <- forkIO $ simpleHTTP nullConf implURL
>        putStrLn "running..."
>        waitForTermination
>        killThread tid
>

</body>
</html>