{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances, TemplateHaskell, TypeFamilies, TypeOperators #-}
module Text.Zwaluw.Strings
{-
    (
    -- * Types
    PrinterParser, RouteError(..), (:-)(..), (<>), (.~)
    
    -- * Running routers
  , parse, unparse
--  , parse1, unparse1
    
    -- * PrinterParser combinators
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
import Text.Zwaluw.Prim
import Text.Zwaluw.Error
import Text.Zwaluw.Pos
import Text.Zwaluw.Combinators
import Text.Zwaluw.HList
import Text.Zwaluw.TH

data StringsPos = StringsPos 
    { string    :: Integer 
    , character :: Integer
    }
    deriving (Eq, Ord, Typeable, Data)

instance Show StringsPos where
    show (StringsPos s c) = "string " ++ show s ++ ", character " ++ show c

instance Position StringsPos where
    initialPos = StringsPos 0 0

addString :: (Integral i) => i -> StringsPos -> StringsPos
addString i (StringsPos seg chr) = StringsPos (seg + (fromIntegral i)) 0

addChar :: (Integral i) => i -> StringsPos -> StringsPos
addChar i (StringsPos seg chr) = StringsPos seg (chr + (fromIntegral i))

instance a ~ b => IsString (PrinterParser (ParseError StringsPos) [String] a b) where
    fromString = lit

-- | a constant string.
lit :: String -> PrinterParser (ParseError StringsPos) [String] r r
lit l = PrinterParser pf sf 
    where
      pf = Parser $ \tok pos ->
           case tok of
             [] -> mkParseError pos [RouteEOF]
             ("":_) | (not $ null l) -> mkParseError pos [RouteEOS]
             (p:ps) ->
                 case stripPrefix l p of
                   (Just p') -> 
                       do [Right ((id, p':ps), addChar (length l) pos)]
                   Nothing -> 
                       mkParseError pos [UnExpect (show p), Expect (show l)]
      sf b = [(( \(s:ss) -> ((l ++ s) : ss)), b)]



infixr 9 </>
-- | equivalent to @f . eops . g@
(</>) :: PrinterParser (ParseError StringsPos) [String] b c -> PrinterParser (ParseError StringsPos) [String] a b -> PrinterParser (ParseError StringsPos) [String] a c
f </> g = f . eops . g

-- | end of path segment
eops :: PrinterParser (ParseError StringsPos) [String] r r
eops = PrinterParser 
       (Parser $ \path pos -> case path of
                   []      -> [Right ((id, []), addString 1 pos)]
--                   [] -> mkParseError pos [RouteEOF]
                   ("":ps) -> 
                          [ Right ((id, ps), addString 1 pos) ]
                   (p:_) -> mkParseError pos [Message $ "path-segment not entirely consumed: " ++ p])
       (\a -> [(("" :), a)])

-- | statisfy a 'Char' predicate
satisfy :: (Char -> Bool) -> PrinterParser (ParseError StringsPos) [String] r (Char :- r)
satisfy p = val
  (Parser $ \tok pos -> 
       case tok of
         []          -> mkParseError pos [RouteEOF]
         ("":ss)     -> mkParseError pos [RouteEOS]
         ((c:cs):ss)
             | p c -> 
                 do [Right ((c, cs : ss), addChar 1 pos )]
             | otherwise -> 
                 do mkParseError pos [SysUnExpect [c]]
  )
  (\c -> [ \paths -> case paths of [] -> [[c]] ; (s:ss) -> ((c:s):ss) | p c ])


-- | satisfy a 'String' predicate. 
--
-- Note: must match the entire remainder of the 'String' in this segment
satisfyStr :: (String -> Bool) -> PrinterParser (ParseError StringsPos) [String] r (String :- r)
satisfyStr p = val
  (Parser $ \tok pos -> 
       case tok of
         []          -> mkParseError pos [RouteEOF]
         ("":ss)     -> mkParseError pos [RouteEOS]
         (s:ss)
             | p s -> 
                 do [Right ((s, "":ss), addString 1 pos )]
             | otherwise -> 
                 do mkParseError pos [SysUnExpect s]
  )
  (\str -> [ \paths -> case paths of [] -> [str] ; (s:ss) -> ((str++s):ss) | p str ])


-- | ascii digits @\'0\'..\'9\'@
digit :: PrinterParser (ParseError StringsPos) [String] r (Char :- r)
digit = satisfy isDigit <?> "a digit 0-9"

-- | matches alphabetic Unicode characters (lower-case, upper-case and title-case letters, 
-- plus letters of caseless scripts and modifiers letters).  (Uses 'isAlpha')
alpha :: PrinterParser (ParseError StringsPos) [String] r (Char :- r)
alpha = satisfy isAlpha <?> "an alphabetic Unicode character"

-- | matches white-space characters in the Latin-1 range. (Uses 'isSpace')
space :: PrinterParser (ParseError StringsPos) [String] r (Char :- r)
space = satisfy isSpace <?> "a white-space character"

-- | any character
anyChar :: PrinterParser (ParseError StringsPos) [String] r (Char :- r)
anyChar = satisfy (const True)

-- | matches the specified character
char :: Char -> PrinterParser (ParseError StringsPos) [String] r (Char :- r)
char c = satisfy (== c) <?> show [c]

-- | matches an 'Int'
int :: PrinterParser (ParseError StringsPos) [String] r (Int :- r)
int = xmaph read (Just . show) (opt (rCons . char '-') . (rList1 digit))

-- | matches an 'Integer'
integer :: PrinterParser (ParseError StringsPos) [String] r (Integer :- r)
integer = xmaph read (Just . show) (opt (rCons . char '-') . (rList1 digit))

-- | matches any 'String'
anyString :: PrinterParser (ParseError StringsPos) [String] r (String :- r)
anyString = val ps ss 
    where
      ps = Parser $ \tok pos ->
           case tok of
             []     -> mkParseError pos [RouteEOF]
             ("":_) -> mkParseError pos [RouteEOS]
             (s:ss) -> [Right ((s, ss), addString 1 pos)]
      ss str = [\ss -> str : ss]

-- | Predicate to test if we have parsed all the strings.
-- Typically used as argument to 'parse1'
isComplete :: [String] -> Bool
isComplete []   = True
isComplete [""] = True
isComplete _    = False
