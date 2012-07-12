{-# LANGUAGE CPP, TemplateHaskell #-}
{- OPTIONS_GHC -optP-include -optPdist/build/autogen/cabal_macros.h -}
module Web.Routes.TH
     ( derivePathInfo
     , derivePathInfo'
     , standard
     ) where

import Control.Monad                 (ap, replicateM)
import Data.Char                     (isUpper, toLower, toUpper)
import Data.List                     (intercalate)
import Data.List.Split               (split, dropInitBlank, keepDelimsL, whenElt)
import Data.Text                     (pack, unpack)
import Language.Haskell.TH
import Text.ParserCombinators.Parsec ((<|>),many1)
import Web.Routes.PathInfo

-- | use Template Haskell to create 'PathInfo' instances for a type.
--
-- > $(derivePathInfo ''SiteURL)
--
-- Uses the 'standard' formatter by default.
derivePathInfo :: Name
               -> Q [Dec]
derivePathInfo = derivePathInfo' standard

-- FIXME: handle when called with a type (not data, newtype)

-- | use Template Haskell to create 'PathInfo' instances for a type.
--
-- This variant allows the user to supply a function that transforms
-- the constructor name to a prettier rendering. It is important that
-- the transformation function generates a unique output for each
-- input. For example, simply converting the string to all lower case
-- is not acceptable, because then 'FooBar' and 'Foobar' would be
-- indistinguishable.
--
-- > $(derivePathInfo' standard ''SiteURL)
--
-- see also: 'standard'
derivePathInfo' :: (String -> String)
                -> Name
                -> Q [Dec]
derivePathInfo' formatter name
    = do c <- parseInfo name
         case c of
           Tagged cons cx keys ->
               do let context = [ mkCtx ''PathInfo [varT key] | key <- keys ] ++ map return cx
                  i <- instanceD (sequence context) (mkType ''PathInfo [mkType name (map varT keys)])
                       [ toPathSegmentsFn cons
                       , fromPathSegmentsFn cons
                       ]
                  return [i]
    where
#if MIN_VERSION_template_haskell(2,4,0)
      mkCtx = classP
#else
      mkCtx = mkType
#endif

      toPathSegmentsFn :: [(Name, Int)] -> DecQ
      toPathSegmentsFn cons
          = do inp <- newName "inp"
               let body = caseE (varE inp) $
                            [ do args <- replicateM nArgs (newName "arg")
                                 let matchCon = conP conName (map varP args)
                                     conStr = formatter (nameBase conName)
                                 match matchCon (normalB (toURLWork conStr args)) []
                                  |  (conName, nArgs) <- cons ]
                   toURLWork :: String -> [Name] -> ExpQ
                   toURLWork conStr args
                       = foldr1 (\a b -> appE (appE [| (++) |] a) b) ([| [pack conStr] |] : [ [| toPathSegments $(varE arg) |] | arg <- args ])
               funD 'toPathSegments [clause [varP inp] (normalB body)  []]
      fromPathSegmentsFn :: [(Name,Int)] -> DecQ
      fromPathSegmentsFn cons
          = do let body = (foldl1 (\a b -> appE (appE [| (<|>) |] a) b)
                            [ parseCon conName nArgs
                            | (conName, nArgs) <- cons])
                   parseCon :: Name -> Int -> ExpQ
                   parseCon conName nArgs = foldl1 (\a b -> appE (appE [| ap |] a) b)
                                                   ([| segment (pack $(stringE (formatter $ nameBase conName))) >> return $(conE conName) |]
                                                   : (replicate nArgs [| fromPathSegments |]))
               funD 'fromPathSegments [clause [] (normalB body) []]

mkType :: Name -> [TypeQ] -> TypeQ
mkType con = foldl appT (conT con)

data Class = Tagged [(Name, Int)] Cxt [Name]

parseInfo :: Name -> Q Class
parseInfo name
    = do info <- reify name
         case info of
           TyConI (DataD cx _ keys cs _)    -> return $ Tagged (map conInfo cs) cx $ map conv keys
           TyConI (NewtypeD cx _ keys con _)-> return $ Tagged [conInfo con] cx $ map conv keys
           _                            -> error "Invalid input"
    where conInfo (NormalC n args) = (n, length args)
          conInfo (RecC n args) = (n, length args)
          conInfo (InfixC _ n _) = (n, 2)
          conInfo (ForallC _ _ con) = conInfo con
#if MIN_VERSION_template_haskell(2,4,0)
          conv (PlainTV nm) = nm
          conv (KindedTV nm _) = nm
#else
          conv = id
#endif

-- | the standard formatter
--
-- Converts @CamelCase@ to @camel-case@.
--
-- see also: 'derivePathInfo' and 'derivePathInfo''
standard :: String -> String
standard =
    intercalate "-" . map (map toLower) . split splitter
  where
    splitter = dropInitBlank . keepDelimsL . whenElt $ isUpper
