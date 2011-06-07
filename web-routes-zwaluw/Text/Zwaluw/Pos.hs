{-# LANGUAGE DeriveDataTypeable, TypeFamilies #-}
module Text.Zwaluw.Pos where

import Data.Data

type family Pos a :: *

class ErrorPosition e where
    getPosition :: e -> Maybe (Pos e)

class Position a where
    initialPos :: a

data XYPos = XYPos Integer Integer
      deriving (Eq, Ord, Read, Show, Typeable, Data)

-- should this really be tied to the error type that uses it ?
-- if so, that affects addY
instance Position XYPos where
    initialPos = XYPos 1 1

addX :: (Integral i) => i -> XYPos -> XYPos
addX i (XYPos x y) = XYPos (x + (fromIntegral i)) y

addY :: (Integral i) => i -> XYPos -> XYPos
addY i (XYPos x y) = XYPos 1 (y + (fromIntegral i))


