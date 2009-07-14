{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module URLT.XMLGenT where

import HSP
import Control.Applicative
import Control.Monad.Identity
import qualified HSX.XMLGenerator as HSX
import URLT

instance (Monad m) => HSX.XMLGen (URLT url m) where
    type HSX.XML (URLT url m) = XML
    newtype HSX.Child (URLT url m) = UChild { unUChild :: XML }
    newtype HSX.Attribute (URLT url m) = UAttr { unUAttr :: Attribute }

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

instance (Monad m) => HSX.EmbedAsAttr (URLT url m) Attribute where
    asAttr = return . (:[]) . UAttr 

instance (Monad m) => HSX.EmbedAsAttr (URLT url m) (Attr String Char) where
    asAttr (n := c)  = asAttr (n := [c])

instance (Monad m) => HSX.EmbedAsAttr (URLT url m) (Attr String String) where
    asAttr (n := str)  = asAttr $ MkAttr (toName n, pAttrVal str)

instance (Monad m) => HSX.EmbedAsAttr (URLT url m) (Attr String Bool) where
    asAttr (n := True)  = asAttr $ MkAttr (toName n, pAttrVal "true")
    asAttr (n := False) = asAttr $ MkAttr (toName n, pAttrVal "false")

instance (Monad m) => HSX.EmbedAsAttr (URLT url m) (Attr String Int) where
    asAttr (n := i)  = asAttr $ MkAttr (toName n, pAttrVal (show i))

instance (Monad m) => EmbedAsChild (URLT url m) Char where
    asChild = XMLGenT . return . (:[]) . UChild . pcdata . (:[])

instance (Monad m) => EmbedAsChild (URLT url m) String where
    asChild = XMLGenT . return . (:[]) . UChild . pcdata

instance (Monad m) => EmbedAsChild (URLT url m) XML where
    asChild = XMLGenT . return . (:[]) . UChild


instance (Monad m) => AppendChild (URLT url m) XML where
 appAll xml children = do
        chs <- children
        case xml of
         CDATA _ _       -> return xml
         Element n as cs -> return $ Element n as (cs ++ (map unUChild chs))

instance (Monad m) => SetAttr (URLT url m) XML where
 setAll xml hats = do
        attrs <- hats
        case xml of
         CDATA _ _       -> return xml
         Element n as cs -> return $ Element n (foldr (:) as (map unUAttr attrs)) cs

instance (Monad m) => XMLGenerator (URLT url m)
