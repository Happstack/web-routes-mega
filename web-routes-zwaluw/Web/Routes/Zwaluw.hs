{-# LANGUAGE TypeOperators #-}
module Web.Routes.Zwaluw 
    ( module Text.Zwaluw
    , module Text.Zwaluw.Strings
    , Router
    , toSite
    ) where

import Text.Zwaluw          -- (PrinterParser(..), ParserError(..), (:-), condenseErrors, parse1, showParserError, unparse1)
import Text.Zwaluw.Strings  -- (StringsPos(..), isComplete)
import Web.Routes           (Site(..))

type Router url = PrinterParser StringsError [String] () (url :- ())

toSite :: ((url -> [(String, String)] -> String) -> url -> a) -- ^ handler function
       -> Router url -- ^ the router
       -> Site url a
toSite handler r@(PrinterParser pf sf) =
    Site { handleSite = handler
         , formatPathSegments =  \url ->
             case unparseStrings r url of
               Nothing -> error "formatPathSegments failed to produce a url"
               (Just ps) -> (ps, [])
         , parsePathSegments = \paths -> mapLeft (showErrors paths) (parseStrings r paths)
         }
    where
      mapLeft f       = either (Left . f) Right
      showErrors paths err = (showParserError showPos err) ++ " while parsing " ++ show paths
      showPos (MajorMinorPos s c) = "path segment " ++ show (s + 1) ++ ", character " ++ show c
