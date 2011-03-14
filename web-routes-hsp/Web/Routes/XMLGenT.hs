{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Web.Routes.XMLGenT where

import HSP
import Control.Applicative ((<$>))
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified HSX.XMLGenerator as HSX
import Web.Routes.RouteT (RouteT, ShowURL(showURLParams), showURL, URL)

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
    pcdataToChild = HSX.xmlToChild . pcdata                                                           

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

instance (Monad m, Functor m) => IsAttrValue (RouteT url m) T.Text where
    toAttrValue = toAttrValue . T.unpack

instance (Monad m, Functor m) => IsAttrValue (RouteT url m) TL.Text where
    toAttrValue = toAttrValue . TL.unpack

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

instance (Monad m, Functor m, IsName n) => (EmbedAsAttr (RouteT url m) (Attr n TL.Text)) where
    asAttr (n := a) = asAttr $ MkAttr (toName n, pAttrVal $ TL.unpack a)

instance (Monad m, Functor m, IsName n) => (EmbedAsAttr (RouteT url m) (Attr n T.Text)) where
    asAttr (n := a) = asAttr $ MkAttr (toName n, pAttrVal $ T.unpack a)

instance (Functor m, Monad m) => EmbedAsAttr (RouteT url m) (Attr String url) where
    asAttr (n := u) = 
        do url <- showURL u
           asAttr $ MkAttr (toName n, pAttrVal url)

instance (Functor m, Monad m) => EmbedAsChild (RouteT url m) Char where
    asChild = XMLGenT . return . (:[]) . UChild . pcdata . (:[])

instance (Functor m, Monad m) => EmbedAsChild (RouteT url m) String where
    asChild = XMLGenT . return . (:[]) . UChild . pcdata

instance (Monad m, Functor m) => (EmbedAsChild (RouteT url m) TL.Text) where
    asChild = asChild . TL.unpack

instance (Monad m, Functor m) => (EmbedAsChild (RouteT url m) T.Text) where
    asChild = asChild . T.unpack

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
    showURLParams url params = XMLGenT $ showURLParams url params
