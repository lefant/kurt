{-# OPTIONS -O2 -Wall -Werror -Wwarn #-}

{-
Copyright (C) 2010 Fabian Linzberger <e@lefant.net>

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.
-}

{- |
   Module     : Data.Goban.Utils
   Copyright  : Copyright (C) 2010 Fabian Linzberger
   License    : GNU GPL, version 3 or above

   Maintainer : Fabian Linzberger <e@lefant.net>
   Stability  : experimental
   Portability: probably

Utilities and Types for Goban Implementation

-}

module Data.Goban.Utils (
                         Goban(..)
                        ,Move(..)
                        ,Color(..)
                        ,Stone(..)
                        ,Vertex
                        ,Score
                        ,adjacentVertices
                        ,allVertices
                        ,stoneColor
                        ,otherColor
                        ,xToLetter
                        ,letterToX
                        ) where

import Data.Char (chr, ord, toUpper)


class Goban a where
    freeVertices :: a -> [Vertex]
    isSuicide :: a -> Stone -> Bool
    neighbourStones :: a -> Stone -> [Stone]
    groupOfStone :: a -> Stone -> [Stone]
    territory :: a -> Color -> Score

data Move = StoneMove Stone
          | Pass Color
            deriving (Eq)

instance Show Move where
    show (StoneMove (Stone ((x, y), _color))) =
        [(xToLetter x)] ++ (show y)
    show (Pass _color) = "pass"


newtype Stone = Stone (Vertex, Color)
    deriving (Show, Eq)

data Color = Black
           | White
             deriving (Show, Eq)

type Vertex = (Int, Int)

type Score = Float



adjacentVertices :: Int -> Vertex -> [Vertex]
adjacentVertices bsize (x, y) =
    filter inBounds [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]
    where
      inBounds (x', y') =
          and [x' > 0, x' <= bsize, y' > 0, y' <= bsize]

allVertices :: Int -> [Vertex]
allVertices n =
    [(x, y) | x <- [1 .. n], y <- [1 .. n]]



-- moveColor :: Move -> Color
-- moveColor (StoneMove stone) = stoneColor stone
-- moveColor (Pass color) = color

stoneColor :: Stone -> Color
stoneColor (Stone (_vertex, color)) = color

otherColor :: Color -> Color
otherColor Black = White
otherColor White = Black



xToLetter :: Int -> Char
xToLetter n =
    if (n < 1) || (n > 25)
    then error "letterToX: n out of bounds"
    else
        if n <= 8
        then chr (n + 64)
        else chr (n + 65)

letterToX :: Char -> Int
letterToX 'i' =
    error "letterToX: the letter i is skipped for coordinates to avoid confusion with j"
letterToX c =
    if (n < 1) || (n > 25)
    then error "letterToX: n out of bounds"
    else
        if n <= 8
        then n
        else n - 1
    where
      n = (ord $ toUpper c) - 64

