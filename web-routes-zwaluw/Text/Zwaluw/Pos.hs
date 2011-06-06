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

instance Position XYPos where
    initialPos = XYPos 0 1

addX :: (Integral i) => i -> XYPos -> XYPos
addX i (XYPos x y) = XYPos (x + (fromIntegral i)) y

addY :: (Integral i) => i -> XYPos -> XYPos
addY i (XYPos x y) = XYPos 0 (y + (fromIntegral i))


