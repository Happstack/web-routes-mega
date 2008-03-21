{-# LANGUAGE DeriveDataTypeable, FlexibleContexts #-}
module Main where

import Control.Concurrent
import Control.Monad.Reader
import Control.Monad.State
import Data.List
import HAppS.Server
import Text.XHtml hiding (method,dir)
import URLT
import Network.URI
import Data.Generics
import Data.Tree

-- * HAppS Handler

handleURL :: (Monad m, Data url) => (url -> URLT url m a) -> url -> String -> m a
handleURL site defaultUrl path =
    do req <-
            case path of
              "/" -> return defaultUrl
              _ -> case urlRead path of
                     Nothing -> fail "could not parse url"
                     (Just url) -> return url
       runReaderT (site req) urlShow

urlShow :: Data a => a -> Link
urlShow t =
    let args = gmapQ urlShow t
    in encode $
    "/" ++ (replicate (length args) '!') 
        ++ showConstr (toConstr t)
        ++ concat args
    where
      encode = escapeURIString isUnescapedInURI

urlRead :: Data a => Link -> Maybe a
urlRead str =
    rewrite str
    where
      rewrite s = 
          case gread $ toParens (evalState toTree (map args (words (map toSpace (decode s)))) ) of
            [(v, "")] -> Just v
            _ -> Nothing
      toSpace '/' = ' '
      toSpace o = o
      args str = 
          let (pluses, rest) = span (== '!') str
          in (length pluses, rest)
      toTree :: State [(Int, String)] (Tree String)
      toTree = 
          do (argCount, constr) <- next
             args <- replicateM argCount toTree
             return $ Node constr args
      toParens (Node constr args) =
          "(" ++ constr ++ (concatMap ((" " ++) . toParens) args) ++ ")"
      decode = unEscapeString
      next :: (MonadState [s] m) => m s
      next = 
          do (x:xs) <- get
             put xs
             return x

-- * Extra

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

-- * Example

data SimpleSite
    = HomePage
    | MyGallery Gallery
    | YourGallery Gallery
      deriving (Read, Show, Data, Typeable, Eq)

simpleSite :: (Monad m) => SimpleSite -> URLT SimpleSite m Html
simpleSite HomePage =
    do myGallery <- nestURL MyGallery $ showURL Thumbnails
       yourGallery <- nestURL YourGallery $ showURL Thumbnails
       return $ defPage (toHtml "go to " +++ br +++
                         (anchor (toHtml "my gallery")) ! [href myGallery ]  +++ br +++
                         (anchor (toHtml "your gallery")) ! [href yourGallery ] 
                        )
simpleSite (MyGallery g) =
    nestURL MyGallery $ gallery "Jeremy" g
simpleSite (YourGallery g) =
    nestURL YourGallery $ gallery "Someone Else" g

data Gallery
    = Thumbnails 
    | ShowImage Int Size
    deriving (Read, Show, Data, Typeable, Eq)

data Size
    = Full
    | Screen
    deriving (Read, Show, Data, Typeable, Eq)

gallery :: (Monad m) => String -> Gallery -> URLT Gallery m Html
gallery name Thumbnails = 
    do img1 <- showURL (ShowImage 1 Full)
       return $ defPage ((toHtml $ "showing " ++ name ++ "'s gallery thumbnails.") +++
                         (anchor (toHtml "image 1") ! [href img1]))
gallery name (ShowImage i s) = return $ defPage (toHtml $ "showing " ++ name ++ "'s image number " ++ show i ++ " at " ++ show s ++ " size.")

implURL :: [ServerPartT IO Response]
implURL =
    [ withRequest $ \rq ->
      return . toResponse =<< handleURL simpleSite HomePage ("/" ++ concat (intersperse "/" (rqPaths rq)))
    ] ++ catchAll

-- default page layout function
defPage :: Html -> Html
defPage thebody =
    ((header 
      (thetitle (toHtml "Simple Site")) {- +++
      (thelink ! [href "./simplesite.css", rel "stylesheet", thetype "text/css"] << noHtml)-} ) +++
     (body thebody))

main = 
    do -- control <- startSystemState entryPoint
       tid <- forkIO $ simpleHTTP nullConf implURL
       putStrLn "running..."
       waitForTermination
       killThread tid
       -- shutdownSystem control
