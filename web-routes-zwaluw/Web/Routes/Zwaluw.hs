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

type Router url = PrinterParser (ParserError StringsPos) [String] () (url :- ())

toSite :: ((url -> [(String, String)] -> String) -> url -> a) 
       -> Router url -- PrinterParser (ParserError StringsPos) [String] () (url :- ()) 
       -> Site url a
toSite handler r@(PrinterParser pf sf) =
    Site { handleSite = handler
         , formatPathSegments =  \url ->
             case unparse1 [] r url of
               Nothing -> error "formatPathSegments failed to produce a url"
               (Just ps) -> (ps, [])
         , parsePathSegments = \paths -> mapLeft (showErrors paths) (parse1 isComplete r paths)
         }
    where
      mapLeft f       = either (Left . f) Right
      showErrors paths errs = (showParserError showPos $ condenseErrors errs) ++ " while parsing " ++ show paths
      showPos (StringsPos s c) = "segment " ++ show (s + 1) ++ ", character " ++ show c

