{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Web.Routes.XMLGenT where

import HSP
import Control.Applicative ((<$>))
import qualified HSX.XMLGenerator as HSX
import Web.Routes.RouteT (RouteT, ShowURL(showURL), URL)

instance (Functor m, Monad m) => HSX.XMLGen (RouteT url m) where
    type HSX.XML (RouteT url m) = XML
    newtype HSX.Child (RouteT url m) = UChild { unUChild :: XML }
    newtype HSX.Attribute (RouteT url m) = UAttr { unUAttr :: Attribute }

    genElement n attrs children = 
        do attribs <- map unUAttr <$> asAttr attrs
           childer <- flattenCDATA . map unUChild <$> asChild children
           HSX.XMLGenT $ return (Element
                              (toName n)
                              attribs
                              childer
                             )
    xmlToChild = UChild

flattenCDATA :: [XML] -> [XML]
flattenCDATA cxml = 
                case flP cxml [] of
                 [] -> []
                 [CDATA _ ""] -> []
                 xs -> xs                       
    where
        flP :: [XML] -> [XML] -> [XML]
        flP [] bs = reverse bs
        flP [x] bs = reverse (x:bs)
        flP (x:y:xs) bs = case (x,y) of
                           (CDATA e1 s1, CDATA e2 s2) | e1 == e2 -> flP (CDATA e1 (s1++s2) : xs) bs
                           _ -> flP (y:xs) (x:bs)

instance (Functor m, Monad m) => HSX.EmbedAsAttr (RouteT url m) Attribute where
    asAttr = return . (:[]) . UAttr 

instance (Functor m, Monad m) => HSX.EmbedAsAttr (RouteT url m) (Attr String Char) where
    asAttr (n := c)  = asAttr (n := [c])

instance (Functor m, Monad m) => HSX.EmbedAsAttr (RouteT url m) (Attr String String) where
    asAttr (n := str)  = asAttr $ MkAttr (toName n, pAttrVal str)

instance (Functor m, Monad m) => HSX.EmbedAsAttr (RouteT url m) (Attr String Bool) where
    asAttr (n := True)  = asAttr $ MkAttr (toName n, pAttrVal "true")
    asAttr (n := False) = asAttr $ MkAttr (toName n, pAttrVal "false")

instance (Functor m, Monad m) => HSX.EmbedAsAttr (RouteT url m) (Attr String Int) where
    asAttr (n := i)  = asAttr $ MkAttr (toName n, pAttrVal (show i))

instance (Functor m, Monad m) => EmbedAsChild (RouteT url m) Char where
    asChild = XMLGenT . return . (:[]) . UChild . pcdata . (:[])

instance (Functor m, Monad m) => EmbedAsChild (RouteT url m) String where
    asChild = XMLGenT . return . (:[]) . UChild . pcdata

instance (Functor m, Monad m) => EmbedAsChild (RouteT url m) XML where
    asChild = XMLGenT . return . (:[]) . UChild

instance (Functor m, Monad m) => EmbedAsChild (RouteT url m) () where
  asChild () = return []

instance (Functor m, Monad m) => AppendChild (RouteT url m) XML where
 appAll xml children = do
        chs <- children
        case xml of
         CDATA _ _       -> return xml
         Element n as cs -> return $ Element n as (cs ++ (map unUChild chs))

instance (Functor m, Monad m) => SetAttr (RouteT url m) XML where
 setAll xml hats = do
        attrs <- hats
        case xml of
         CDATA _ _       -> return xml
         Element n as cs -> return $ Element n (foldr (:) as (map unUAttr attrs)) cs

instance (Functor m, Monad m) => XMLGenerator (RouteT url m)


instance (ShowURL m) => ShowURL (XMLGenT m) where
    type URL (XMLGenT m) = URL m
    showURL url = XMLGenT $ showURL url
