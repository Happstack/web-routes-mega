{-# LANGUAGE TypeOperators, ScopedTypeVariables #-}
module Web.Routes.Regular where

import Control.Applicative hiding ((<|>))
import Data.Char (toLower)
import Generics.Regular
import Text.ParserCombinators.Parsec.Prim
import Text.ParserCombinators.Parsec.Combinator
import Web.Routes.PathInfo (PathInfo(fromPathSegments, toPathSegments), URLParser, segment)

class GToURL f where
   gtoPathSegments   :: f a -> [String]
   gfromPathSegments :: URLParser (f a)

instance PathInfo a => GToURL (K a) where   
  gtoPathSegments (K a) = toPathSegments a
  gfromPathSegments = K <$> fromPathSegments 

instance (GToURL f, GToURL g) => GToURL (f :+: g) where
   gtoPathSegments   (L x) = gtoPathSegments x
   gtoPathSegments   (R y) = gtoPathSegments y
   gfromPathSegments = try (L <$> gfromPathSegments) <|> (R <$> gfromPathSegments)
                          
instance (GToURL f, GToURL g) => GToURL (f :*: g) where
  gtoPathSegments (x :*: y) = gtoPathSegments x ++ gtoPathSegments y
  gfromPathSegments =
    do x <- gfromPathSegments
       y <- gfromPathSegments
       return (x :*: y)

instance GToURL U where
  gtoPathSegments U = []
  gfromPathSegments = eof >> return U

instance GToURL f => GToURL (S s f) where
   gtoPathSegments   (S x) = gtoPathSegments x
   gfromPathSegments       = S <$> gfromPathSegments

lower :: String -> String
lower = map toLower

instance forall c f. (Constructor c, GToURL f) => GToURL (C c f) where
   gtoPathSegments c@(C x)  = (lower $ conName c) : gtoPathSegments x
   gfromPathSegments = 
     let constr = undefined :: C c f r
     in do segment (lower $ conName constr)
           C <$> gfromPathSegments
