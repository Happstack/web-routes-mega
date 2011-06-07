{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances, TemplateHaskell, TypeFamilies, TypeOperators #-}
module Text.Zwaluw.Strings
    (
    -- * Position information
    StringsPos(..), addString, addChar
    -- * Combinators
    , (</>), alpha, anyChar, anyString, char, digit, eops, int
    , integer, lit, satisfy, satisfyStr, space
    -- * Misc
    , isComplete
    )
    where

import Prelude                 hiding ((.), id, (/))
import Control.Category        (Category((.), id))
import Data.Char               (isAlpha, isDigit, isSpace)
import Data.Data               (Data, Typeable)
import Data.List               (stripPrefix)
import Data.String             (IsString(..))
import Text.Zwaluw.Combinators (opt, rCons, rList1)
import Text.Zwaluw.Error       (ParserError(..),ErrorMsg(..), (<?>), mkParserError)
import Text.Zwaluw.HList       ((:-)(..))
import Text.Zwaluw.Pos         (InitialPosition(..))
import Text.Zwaluw.Prim        (Parser(..), PrinterParser(..), xmaph, val)

data StringsPos = StringsPos 
    { string    :: Integer 
    , character :: Integer
    }
    deriving (Eq, Ord, Typeable, Data)

instance Show StringsPos where
    show (StringsPos s c) = "string " ++ show s ++ ", character " ++ show c

instance InitialPosition StringsPos where
    initialPos = StringsPos 0 0

addString :: (Integral i) => i -> StringsPos -> StringsPos
addString i (StringsPos seg chr) = StringsPos (seg + (fromIntegral i)) 0

addChar :: (Integral i) => i -> StringsPos -> StringsPos
addChar i (StringsPos seg chr) = StringsPos seg (chr + (fromIntegral i))

instance a ~ b => IsString (PrinterParser (ParserError StringsPos) [String] a b) where
    fromString = lit

-- | a constant string.
lit :: String -> PrinterParser (ParserError StringsPos) [String] r r
lit l = PrinterParser pf sf 
    where
      pf = Parser $ \tok pos ->
           case tok of
             [] -> mkParserError pos [RouteEOF]
             ("":_) | (not $ null l) -> mkParserError pos [RouteEOS]
             (p:ps) ->
                 case stripPrefix l p of
                   (Just p') -> 
                       do [Right ((id, p':ps), addChar (length l) pos)]
                   Nothing -> 
                       mkParserError pos [UnExpect (show p), Expect (show l)]
      sf b = [(( \(s:ss) -> ((l ++ s) : ss)), b)]



infixr 9 </>
-- | equivalent to @f . eops . g@
(</>) :: PrinterParser (ParserError StringsPos) [String] b c -> PrinterParser (ParserError StringsPos) [String] a b -> PrinterParser (ParserError StringsPos) [String] a c
f </> g = f . eops . g

-- | end of path segment
eops :: PrinterParser (ParserError StringsPos) [String] r r
eops = PrinterParser 
       (Parser $ \path pos -> case path of
                   []      -> [Right ((id, []), addString 1 pos)]
--                   [] -> mkParserError pos [RouteEOF]
                   ("":ps) -> 
                          [ Right ((id, ps), addString 1 pos) ]
                   (p:_) -> mkParserError pos [Message $ "path-segment not entirely consumed: " ++ p])
       (\a -> [(("" :), a)])

-- | statisfy a 'Char' predicate
satisfy :: (Char -> Bool) -> PrinterParser (ParserError StringsPos) [String] r (Char :- r)
satisfy p = val
  (Parser $ \tok pos -> 
       case tok of
         []          -> mkParserError pos [RouteEOF]
         ("":ss)     -> mkParserError pos [RouteEOS]
         ((c:cs):ss)
             | p c -> 
                 do [Right ((c, cs : ss), addChar 1 pos )]
             | otherwise -> 
                 do mkParserError pos [SysUnExpect [c]]
  )
  (\c -> [ \paths -> case paths of [] -> [[c]] ; (s:ss) -> ((c:s):ss) | p c ])


-- | satisfy a 'String' predicate. 
--
-- Note: must match the entire remainder of the 'String' in this segment
satisfyStr :: (String -> Bool) -> PrinterParser (ParserError StringsPos) [String] r (String :- r)
satisfyStr p = val
  (Parser $ \tok pos -> 
       case tok of
         []          -> mkParserError pos [RouteEOF]
         ("":ss)     -> mkParserError pos [RouteEOS]
         (s:ss)
             | p s -> 
                 do [Right ((s, "":ss), addString 1 pos )]
             | otherwise -> 
                 do mkParserError pos [SysUnExpect s]
  )
  (\str -> [ \paths -> case paths of [] -> [str] ; (s:ss) -> ((str++s):ss) | p str ])


-- | ascii digits @\'0\'..\'9\'@
digit :: PrinterParser (ParserError StringsPos) [String] r (Char :- r)
digit = satisfy isDigit <?> "a digit 0-9"

-- | matches alphabetic Unicode characters (lower-case, upper-case and title-case letters, 
-- plus letters of caseless scripts and modifiers letters).  (Uses 'isAlpha')
alpha :: PrinterParser (ParserError StringsPos) [String] r (Char :- r)
alpha = satisfy isAlpha <?> "an alphabetic Unicode character"

-- | matches white-space characters in the Latin-1 range. (Uses 'isSpace')
space :: PrinterParser (ParserError StringsPos) [String] r (Char :- r)
space = satisfy isSpace <?> "a white-space character"

-- | any character
anyChar :: PrinterParser (ParserError StringsPos) [String] r (Char :- r)
anyChar = satisfy (const True)

-- | matches the specified character
char :: Char -> PrinterParser (ParserError StringsPos) [String] r (Char :- r)
char c = satisfy (== c) <?> show [c]

-- | matches an 'Int'
int :: PrinterParser (ParserError StringsPos) [String] r (Int :- r)
int = xmaph read (Just . show) (opt (rCons . char '-') . (rList1 digit))

-- | matches an 'Integer'
integer :: PrinterParser (ParserError StringsPos) [String] r (Integer :- r)
integer = xmaph read (Just . show) (opt (rCons . char '-') . (rList1 digit))

-- | matches any 'String'
anyString :: PrinterParser (ParserError StringsPos) [String] r (String :- r)
anyString = val ps ss 
    where
      ps = Parser $ \tok pos ->
           case tok of
             []     -> mkParserError pos [RouteEOF]
             ("":_) -> mkParserError pos [RouteEOS]
             (s:ss) -> [Right ((s, ss), addString 1 pos)]
      ss str = [\ss -> str : ss]

-- | Predicate to test if we have parsed all the strings.
-- Typically used as argument to 'parse1'
isComplete :: [String] -> Bool
isComplete []   = True
isComplete [""] = True
isComplete _    = False
