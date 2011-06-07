{-# LANGUAGE TypeOperators #-}
module Web.Routes.Zwaluw 
    ( module Text.Zwaluw
    , module Text.Zwaluw.Strings
    , Router
    , toSite
    ) where

import Web.Routes (Site(..))
import Text.Zwaluw
import Text.Zwaluw.Strings

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
         , parsePathSegments = mapLeft (showParserError showPos . condenseErrors) . (parse1 isComplete r)
         }
    where
      mapLeft f = either (Left . f) Right
      showPos (StringsPos s c) = "segment " ++ show (s + 1) ++ ", character " ++ show c

