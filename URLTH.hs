{-# LANGUAGE TemplateHaskell #-}
module URLTH where

import Control.Monad
import Language.Haskell.TH
import Control.Monad.Consumer

class AsURL a where
    toURLS :: a -> ShowS
    fromURLC :: Consumer String a

toURL :: (AsURL a) => a -> String
toURL u = '/' : toURLS u ""

fromURL :: (AsURL a) => String -> a
fromURL str = 
    fst $ runConsumer (words $ map (\c -> if c == '/' then ' ' else c) str) fromURLC

-- FIXME: handle unexpected end of input
-- FIXME: handle invalid input
-- FIXME: handle when called with a type (not data, newtype)
deriveAsURL :: Name -> Q [Dec]
deriveAsURL name
    = do c <- parseInfo name
         case c of
           Tagged cons cx keys ->
               do let context = [ mkType ''AsURL [varT key] | key <- keys ] ++ map return cx
                  i <- instanceD (sequence context) (mkType ''AsURL [mkType name (map varT keys)])
                       [ toURLFn cons 
                       , fromURLCFn cons
                       ]
                  return [i]
    where
      toURLFn :: [(Name, Int)] -> DecQ
      toURLFn cons 
          = do inp <- newName "inp"
               let body = caseE (varE inp) $
                            [ do args <- replicateM nArgs (newName "arg")
                                 let matchCon = conP conName (map varP args)
                                     conStr = (nameBase conName) ++ "/"
                                 match matchCon (normalB (toURLWork conStr args)) []
                                  |  (conName, nArgs) <- cons ]
                   toURLWork :: String -> [Name] -> ExpQ
                   toURLWork conStr args
                       = foldr1 (\ a b -> appE (appE [| (.) |] a) b) ([| ((++) conStr) |] : [ [| toURLS $(varE arg) |] | arg <- args ])
               funD 'toURLS [clause [varP inp] (normalB body)  []]
      fromURLCFn :: [(Name,Int)] -> DecQ
      fromURLCFn cons
          = do let body = 
                       do c <- newName "c"
                          doE [ bindS (varP c) (varE 'next)
                              , noBindS $ caseE (varE c)
                                        [ do args <- replicateM nArgs (newName "arg")
                                             match (conP (mkName "Just") [litP $ stringL (nameBase conName) ])
                                                       (normalB (fromURLWork conName args)) []
                                          | (conName, nArgs) <- cons
                                        ]

                              ]
                   fromURLWork :: Name -> [Name] -> ExpQ
                   fromURLWork conName args
                       = doE $ [ bindS (varP arg) [| fromURLC |] | arg <- args ] ++
                               [ noBindS [| return $(foldl appE (conE conName) (map varE args)) |] ]
               funD 'fromURLC [clause [] (normalB body) []]


mkType :: Name -> [TypeQ] -> TypeQ
mkType con = foldl appT (conT con)

data Class = Tagged [(Name, Int)] Cxt [Name]


parseInfo :: Name -> Q Class
parseInfo name
    = do info <- reify name
         case info of
           TyConI (DataD cx _ keys cs _)    -> return $ Tagged (map conInfo cs) cx keys
           TyConI (NewtypeD cx _ keys con _)-> return $ Tagged [conInfo con] cx keys
           _                            -> error "Invalid input"
    where conInfo (NormalC n args) = (n, length args)
          conInfo (RecC n args) = (n, length args)
          conInfo (InfixC _ n _) = (n, 2)
          conInfo (ForallC _ _ con) = conInfo con
