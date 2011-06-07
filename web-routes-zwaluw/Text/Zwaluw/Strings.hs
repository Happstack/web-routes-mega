{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances, TemplateHaskell, TypeFamilies, TypeOperators #-}
module Text.Zwaluw.Strings
{-
    (
    -- * Types
    Router, RouteError(..), (:-)(..), (<>), (.~)
    
    -- * Running routers
  , parse, unparse
--  , parse1, unparse1
    
    -- * Router combinators
  , pure, xmap, xmaph
  , val, readshow, lit, push
  , opt, duck, satisfy, rFilter, printAs
  , manyr, somer, chainr, chainr1 
  , manyl, somel, chainl, chainl1
  
    -- * Built-in routers
  , int, string -- ,char, digit, hexDigit
  , (</>),
  {-
  , rNil, rCons, rList, rListSep
  , rPair
  , rLeft, rRight, rEither
  , rNothing, rJust, rMaybe
  , rTrue, rFalse
-}
    ) -}
    where

import Prelude hiding ((.), id, (/))
import Control.Arrow (first)
import Control.Monad (guard)
import Control.Monad.State
-- import Control.Monad.Trans.Error (Error(..))
import Control.Category
import Control.Monad.Error
-- import Control.Monad.Trans.Error (ErrorList(..))
import Data.Char
import Data.Data
import Data.Monoid
import Data.Char (isDigit, isHexDigit, intToDigit, digitToInt)
import Data.List (intercalate, sort, nub, stripPrefix)
import Data.String (IsString(..))
import Text.Zwaluw.Core
import Text.Zwaluw.Error
import Text.Zwaluw.Pos
import Text.Zwaluw.Combinators
import Text.Zwaluw.HList
import Text.Zwaluw.TH

instance a ~ b => IsString (Router RouteError [String] a b) where
    fromString = lit

-- | Routes a constant string.
lit :: String -> Router RouteError [String] r r
lit l = Router pf sf 
    where
      pf = Parser $ \tok pos ->
           case tok of
             [] -> throwRouteError pos [RouteEOF]
             ("":_) | (not $ null l) -> throwRouteError pos [RouteEOS]
             (p:ps) ->
                 case stripPrefix l p of
                   (Just p') -> 
                       do [Right ((id, p':ps), addX (length l) pos)]
                   Nothing -> 
                       throwRouteError pos [UnExpect (show p), Expect (show l)]
      sf b = [(( \(s:ss) -> ((l ++ s) : ss)), b)]



infixr 9 </>
(</>) :: Router RouteError [String] b c -> Router RouteError [String] a b -> Router RouteError [String] a c
f </> g = f . eops . g

eops :: Router RouteError [String] r r
eops = Router 
       (Parser $ \path pos -> case path of
                   []      -> [Right ((id, []), addY 1 pos)]
--                   [] -> throwRouteError pos [RouteEOF]
                   ("":ps) -> 
                          [ Right ((id, ps), addY 1 pos) ]
                   (p:_) -> throwRouteError pos [Message $ "path-segment not entirely consumed: " ++ p])
       (\a -> [(("" :), a)])

satisfy :: (Char -> Bool) -> Router RouteError [String] r (Char :- r)
satisfy p = val
  (Parser $ \tok pos -> 
       case tok of
         []          -> throwRouteError pos [RouteEOF]
         ("":ss)     -> throwRouteError pos [RouteEOS]
         ((c:cs):ss)
             | p c -> 
                 do [Right ((c, cs : ss), addX 1 pos )]
             | otherwise -> 
                 do throwRouteError pos [SysUnExpect [c]]
  )
  (\c -> [ \paths -> case paths of [] -> [[c]] ; (s:ss) -> ((c:s):ss) | p c ])


-- check that the remainder of the 'String' in this segment satisfies a prefix
satisfyStr :: (String -> Bool) -> Router RouteError [String] r (String :- r)
satisfyStr p = val
  (Parser $ \tok pos -> 
       case tok of
         []          -> throwRouteError pos [RouteEOF]
         ("":ss)     -> throwRouteError pos [RouteEOS]
         (s:ss)
             | p s -> 
                 do [Right ((s, "":ss), addY 1 pos )]
             | otherwise -> 
                 do throwRouteError pos [SysUnExpect s]
  )
  (\str -> [ \paths -> case paths of [] -> [str] ; (s:ss) -> ((str++s):ss) | p str ])


digit :: Router RouteError [String] r (Char :- r)
digit = satisfy isDigit <?> "a digit 0-9"

-- | matches alphabetic Unicode characters (lower-case, upper-case and title-case letters, plus letters of caseless scripts and modifiers letters).  (Uses 'isAlpha')
alpha :: Router RouteError [String] r (Char :- r)
alpha = satisfy isAlpha <?> "an alphabetic Unicode character"

anyChar :: Router RouteError [String] r (Char :- r)
anyChar = satisfy (const True)

char :: Char -> Router RouteError [String] r (Char :- r)
char c = satisfy (== c) <?> show [c]

int :: Router RouteError [String] r (Int :- r)
int = xmaph read (Just . show) (opt (rCons . char '-') . (rList1 digit))

-- | Routes any string.
-- FIXME: not sure the ss function is really doing the right thing
anyString :: Router RouteError [String] r (String :- r)
anyString = val ps ss 
    where
      ps = Parser $ \tok pos ->
           case tok of
             []     -> throwRouteError pos [RouteEOF]
             ("":_) -> throwRouteError pos [RouteEOS]
             (s:ss) -> [Right ((s, ss), addY 1 pos)]
      ss str = [\ss -> str : ss]

-- | @r \`printAs\` s@ uses ther serializer of @r@ to test if serializing succeeds,
--   and if it does, instead serializes as @s@. 
printAs :: Router e [String] a b -> String -> Router e [String] a b
printAs r s = r { ser = map (first (const (s :))) . take 1 . ser r }

isComplete :: [String] -> Bool
isComplete []   = True
isComplete [""] = True
isComplete _    = False

      

        