module Main where

import Control.Concurrent
import Control.Monad.Reader
import HAppS.Server
import Text.XHtml hiding (method,dir)
import URLT
import Network.URI


impl :: [ServerPartT IO String]
impl = [ anyRequest $ ok "hello, world" ]    

data SimpleSite
    = HomePage
    | MyGallery Gallery
      deriving (Read, Show)

simpleSite :: (Monad m) => SimpleSite -> URLT SimpleSite m Html
simpleSite HomePage =
    do gallery <- nestURL MyGallery $ showURL Thumbnails
       return $ defPage (toHtml "go to " +++
                         (anchor (toHtml "my gallery")) ! [href gallery ] )
simpleSite (MyGallery g) =
    nestURL MyGallery $ gallery "Jeremy" g

data Gallery
    = Thumbnails 
    | ShowImage Int
    deriving (Read, Show)

gallery :: (Monad m) => String -> Gallery -> URLT Gallery m Html
gallery name Thumbnails = 
    do img1 <- showURL (ShowImage 1)
       return $ defPage ((toHtml $ "showing " ++ name ++ "'s gallery thumbnails.") +++
                         (anchor (toHtml "image 1") ! [href img1]))
gallery name (ShowImage i) = return $ defPage (toHtml $ "showing " ++ name ++ "'s image number " ++ show i)

{-
runSite :: (Read url, Show url) => (url -> URLT url (CGIT IO) Html) -> url -> IO ()
runSite site defaultUrl =
    runCGI $ do queryStr <- liftM (fromMaybe "")  $ getVar "QUERY_STRING"
                scriptName <- liftM fromJust $ getVar "SCRIPT_NAME"
                html <- handleURL scriptName site defaultUrl queryStr
                output (renderHtml html)
-}
-- handleURL :: (Read url, Show url) => (url -> URLT url m Html) -> url -> String -> m Html
handleURL site defaultUrl queryStr =
    do req <-
            case reads (decode queryStr) of
              [(url,"")] -> return url
              _ -> return defaultUrl
       runReaderT (site req) (("?" ++) . show)
    where
      decode = unEscapeString . dropQ
      dropQ ('/':'?':rest) = rest
      dropQ o = o

implURL :: [ServerPartT IO Response ]
implURL =
    [ uriRest $ \str ->
      method GET $ ok . toResponse =<< handleURL simpleSite HomePage str
    ] ++ catchAll

catchAll = 
    [ withRequest $ \rq -> notFound ( (toResponse (prettyRequest rq)))
    ]
    where
      prettyRequest :: Request -> Html
      prettyRequest (Request method paths query inputs cookies version headers body' peer)
          = thehtml ((thetitle (toHtml "404"))  +++
                     (body ((h1 (toHtml "Requested object not found.")) +++
                            (table
                             ((tr (td (toHtml "method") +++ (td (toHtml (show method))))) +++
                              (tr (td (toHtml "paths") +++ (td (toHtml (show paths))))) +++
                              (tr (td (toHtml "query") +++ (td (toHtml query)))) +++
                              (tr (td (toHtml "inputs") +++ (td (toHtml (show inputs))))) +++
                              (tr (td (toHtml "cookies") +++ (td (toHtml (show cookies))))) +++
                              (tr (td (toHtml "version") +++ (td (toHtml (show version))))) +++
                              (tr (td (toHtml "headers") +++ (td (toHtml (show headers))))) +++
                              (tr (td (toHtml "peer") +++ (td (toHtml (show peer))))) +++
                              (tr (td (toHtml "body") +++ (td (toHtml (show body')))))
                             )
                            )
                           ) 
                     ) 
                    )


-- default page layout function
defPage :: Html -> Html
defPage thebody =
    ((header 
      (thetitle (toHtml "Simple Site")) +++
      (thelink ! [href "./simplesite.css", rel "stylesheet", thetype "text/css"] << noHtml)) +++
     (body thebody))

main = do -- control <- startSystemState entryPoint
          tid <- forkIO $ simpleHTTP nullConf implURL
          putStrLn "running..."
          waitForTermination
          killThread tid
          -- shutdownSystem control
