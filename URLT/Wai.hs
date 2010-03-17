module URLT.Wai where

import Control.Applicative.Error
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import Network.Wai
import Network.Wai.Enumerator

handleWai :: (url -> String) -> (String -> Failing url) -> ((url -> String) -> url -> Application) -> Application
handleWai fromUrl toUrl handler =
  \request ->
     do let fUrl = toUrl $ S.unpack $ pathInfo request
        case fUrl of
          (Failure errs) -> return $ Response Status404 [] $ Right $ fromLBS (L.pack $ unlines errs)
          (Success url) -> handler fromUrl url request
