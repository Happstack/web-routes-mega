module URLT.Wai where

import Control.Applicative.Error
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import Network.Wai
import Network.Wai.Enumerator
import URLT.PathInfo 
import URLT.HandleT
import URLT.Monad (URLT, runURLT)

handleWai_ :: (url -> String) -> (String -> Failing url) -> String -> ((url -> String) -> url -> Application) -> Application
handleWai_ fromUrl toUrl approot handler =
  \request ->
     do let fUrl = toUrl $ stripOverlap approot $ S.unpack $ pathInfo request
        case fUrl of
          (Failure errs) -> return $ Response Status404 [] $ Right $ fromLBS (L.pack $ unlines errs)
          (Success url) -> handler (showString approot . fromUrl) url request
          
handleWai_1 :: (url -> String) -> (String -> Failing url) -> String -> ([ErrorMsg] -> Application) -> ((url -> String) -> url -> Application) -> Application
handleWai_1 fromUrl toUrl approot handleError handler =
  \request ->
     do let fUrl = toUrl $ stripOverlap approot $ S.unpack $ pathInfo request
        case fUrl of
          (Failure errs) -> handleError errs request
          (Success url)  -> handler (showString approot . fromUrl) url request
          
handleWai_2 :: (url -> String) -> (String -> Failing url) -> String -> ((url -> String) -> url -> Application) -> (Request -> IO (Failing Response))
handleWai_2 fromUrl toUrl approot handler =
  \request ->
     do let fUrl = toUrl $ stripOverlap approot $ S.unpack $ pathInfo request
        case fUrl of
          (Failure errs) -> return (Failure errs)
          (Success url)  -> fmap Success $ handler (showString approot . fromUrl) url request          

handleWai :: (PathInfo url) => String -> ((url -> String) -> url -> Application) -> Application
handleWai approot handler = handleWai_ toPathInfo fromPathInfo approot handler

handleWaiURLT_ :: (url -> String) -> (String -> Failing url) -> String -> (url -> Request -> URLT url IO Response) -> Application
handleWaiURLT_  toPathInfo fromPathInfo approot handler =
   handleWai_ toPathInfo fromPathInfo approot (\toPathInfo' url request -> runURLT (handler url request) toPathInfo') 

handleWaiURLT :: (PathInfo url) => String -> (url -> Request -> URLT url IO Response) -> Application
handleWaiURLT approot handler = handleWaiURLT_ toPathInfo fromPathInfo approot handler

waiSite :: Site url String Application -> String -> Application
waiSite site approot = handleWai_ (formatLink site) (withDefault site) approot (handleLink site) 
