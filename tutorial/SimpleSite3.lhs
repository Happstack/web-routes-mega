<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html>
<head>
<title>404 No More!, Part III</title>
<link type='text/css' rel='stylesheet' href='hscolour.css' />
</head>
<body>
First some header stuff.

> {-# LANGUAGE DeriveDataTypeable, FlexibleContexts #-}
> module Main where


> import Control.Concurrent
> import Control.Monad.Trans
> import Control.Monad.Reader
> import Control.Monad.State
> import Data.Tree
> import Data.List
> import Data.Generics
> import HAppS.Server hiding (method, dir)
> import Text.XHtml
> import Network.URI

<h1>404 No More!, Part III</h1>
 (<i><a href='./SimpleSite1.html'>Part I</a> and <a href='./SimpleSite2.html'>Part II</a></i>)
 <p>In this part, we will make some minor modifications to the
 <code>Link</code> monad so that it is more usuable in real world
 applications. These modifications include:</p>
 <ol>
 <li>Make the Link monad be a monad transformer instead, so that we
 can mix in IO and other monads.</li>
 <li>Making the generated links a bit more user and search engine friendly.</li>
 <li>Allow the generated link to be something other than a <code>String</code>.</li>
 </ol>

 <h2>Generalized <code>LinkT</code> type</h2>

 <p>The first thing we do is base our code on the Reader monad
 transformer instead of the plain Reader monad. We also make the type
 of displayed link polymorphic.</p>

> type LinkT link display m a = ReaderT (link -> display) m a

 <p>The <code>LinkT</code> type has for parameters:</p>
 <dl>
   <dt><code>link</code></dt><dd>Our navigation type<dd>
   <dt><code>display</code></dt><dd>The type of a link which has been converted to a showable form</dd>
   <dt><code>m</code></dt><dd>An arbitrary monad we wish to transform.</dd>
   <dt><code>a</code></dt><dd>The return type</dd>
 </dl>                          
    
 <p>The <code>showLink</code> and <code>nestLink</code> functions are
 the same as before, but with a slightly different type signatures</p>

> showLink :: (Monad m) => link -> LinkT link display m display
> showLink url =
>    do showF <- ask
>       return (showF url)

> nestLink :: (link2 -> link1) -> LinkT link2 display m a -> LinkT link1 display m a
> nestLink b = withReaderT (. b)
 
 <h2>Prettier Links</h2

 <p>The links we generated before were pretty ugly. They looked
 something like this:</p>

 <code>http://localhost:8000/MyGallery%20(ShowImage%201%20Full)</code>

 <p>Instead of using <code>read</code> and <code>show</code>, we can
 supply fancier formating/parsing algorithms. The following two
 functions will generate and parsing links which look more like:</p>

 <code>http://localhost:8000/!MyGallery/!!ShowImage/1/Full</code>

 <p>Unfortunately, the following code is quite broken and does not
 play well with others. For example, if a link points to a .css file,
 and the .css file references <code>image.png</code>, the browser will try to rewrite
 the path replacing whatever comes after the last / with
 <code>image.png.</code> So, consider this section a work in
 progress. (In fact, consider everything in this series a work in
 progress).

> type Link = String

> prettyFormatLink :: Data a => a -> Link
> prettyFormatLink t =
>     let args = gmapQ prettyFormatLink t
>     in encode $
>     "/" ++ (replicate (length args) '!') 
>         ++ showConstr (toConstr t)
>         ++ concat args
>     where
>       encode = escapeURIString isUnescapedInURI

> prettyParseLink :: Data a => Link -> Maybe a
> prettyParseLink str =
>     rewrite str
>     where
>       rewrite s = 
>           case gread $ toParens (evalState toTree (map args (words (map toSpace (decode s)))) ) of
>             [(v, "")] -> Just v
>             _ -> Nothing
>       toSpace '/' = ' '
>       toSpace o = o
>       args argStr =
>           let (pluses, rest) = span (== '!') argStr
>           in (length pluses, rest)
>       toTree :: State [(Int, String)] (Tree String)
>       toTree = 
>           do (argCount, constr) <- next
>              args <- replicateM argCount toTree
>              return $ Node constr args
>       toParens (Node constr args) =
>           "(" ++ constr ++ (concatMap ((" " ++) . toParens) args) ++ ")"
>       decode = unEscapeString
>       next :: (MonadState [s] m) => m s
>       next = 
>           do (x:xs) <- get
>              put xs
>              return x

 <h2>What's Left To Do?</h2>
 
 <p>The current implementation only looks at the path portion of the
 URL. We have not considered differentiating between GET and POST
 requests, etc. This can be done in the site code -- but perhaps it
 could be explicitly annotated in the data type you create to
 represent your site navigation.</p>

 <p>link forwarding/migration</p>

 <h2>The rest of the example</h2>                      

 <p>We can now convert our example to use the new type.</p>

> data OurSite 
>     = HomePage
>     | MyGallery Gallery
>     | YourGallery Gallery
>       deriving (Data, Typeable)

> data Gallery
>    = Thumbnails 
>    | ShowImage Int Size
>    deriving (Data, Typeable)

> data Size
>    = Full
>    | Screen
>    deriving (Data, Typeable, Show)

> -- dummy implementation for didactic purposes
> gallery :: (Monad m) => String -> Gallery -> LinkT Gallery Link m Html
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


> ourSite :: (Monad m) => OurSite -> LinkT OurSite Link m Html
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


>
> data Site link url m a
>     = Site { handleLink  :: link -> LinkT link url m a
>            , defaultPage :: link
>            , formatLink  :: link -> url
>            , parseLink   :: url -> Maybe link 
>            }


> -- runSite :: (Monad m) => Site link Link m a -> Link -> m (Maybe a)
> runSite site linkStr =
>     let mLink = 
>             case linkStr of
>                  "" -> Just (defaultPage site)
>                  _ -> (parseLink site) linkStr
>     in
>       case mLink of
>         Nothing -> return Nothing
>         (Just lnk) -> return . Just =<< runReaderT ((handleLink site) lnk) (formatLink site)


> ourSiteSpec :: (Monad m) => Site OurSite Link m Html
> ourSiteSpec =
>     Site { handleLink = ourSite
>          , defaultPage = HomePage
>          , formatLink = prettyFormatLink -- escapeURIString isUnescapedInURI . show
>          , parseLink = prettyParseLink -- readLink
>          }

> -- * Boilerplate code for running ourSite via HAppS. 
> -- Easily adaptable to Network.CGI, etc.

> implURL :: (ToMessage a) => Site link Link (WebT IO) a -> [ServerPartT IO Response]
> implURL siteSpec =
>     [ withRequest $ \rq ->
>           let link = (concat (intersperse "/" (rqPaths rq)))
>           in
>             do lift $ print link
>                return . toResponse =<< runSite siteSpec link
>     ]

> main :: IO ()
> main = 
>     do tid <- forkIO $ simpleHTTP nullConf (implURL ourSiteSpec)
>        putStrLn "running..."
>        waitForTermination
>        killThread tid
>

</body>
</html>