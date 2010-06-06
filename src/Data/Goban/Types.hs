{-# OPTIONS -O2 -Wall -Werror -Wwarn #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

{- |
   Module     : Data.Goban.Types
   Copyright  : Copyright (C) 2010 Fabian Linzberger
   License    : GNU GPL, version 3 or above

   Maintainer : Fabian Linzberger <e@lefant.net>
   Stability  : experimental
   Portability: probably

General low-level types for Goban Implementation

-}

module Data.Goban.Types ( Move(..)
                        , VertexState(..)
                        , VertexSet
                        , Stone(..)
                        , Color(..)
                        , Vertex
                        , Coord1
                        , Boardsize
                        , Score

                        , moveColor
                        , isStoneMove

                        , allVertices
                        , borderVertices
                        , adjacentVertices
                        -- , adjacentVerticesInBounds
                        , diagonalVertices

                        , otherColor
                        , showColorForBoard

                        , gtpShowMove
                        , gtpShowVertex
                        , letterToX
                        , xToLetter
                        ) where


import Data.Char (chr, ord, toUpper)
import Data.Set (Set)
import Data.Tree.UCT.GameTree (UCTMove)



data Move = Move Stone
          | Pass Color
          | Resign Color
            deriving (Eq, Ord)

instance Show Move where
    show (Move stone) = show stone
    show (Pass color) = show color ++ " pass"
    show (Resign color) = show color ++ " resign"

instance UCTMove Move



data Stone = Stone { stoneVertex :: !Vertex
                   , stoneColor  :: !Color
                   }
           deriving (Eq, Ord)

instance Show Stone where
    show (Stone vertex color) =
        (show color) ++ " " ++ gtpShowVertex vertex



-- this should be some kind of enum instance
-- so we can do toEnum / fromEnum instead of stateToWord in Goban implementations
data VertexState = Colored Color | Empty | EmptyKoBlocked | Border
                 deriving (Eq, Ord)

instance Show VertexState where
    show (Colored color) = showColorForBoard color
    show Empty = "."
    show EmptyKoBlocked = "K"
    show Border = " "



data Color = Black
           | White
             deriving (Eq, Ord, Enum)

instance Show Color where
    show Black = "b"
    show White = "w"

showColorForBoard :: Color -> String
showColorForBoard Black = "x"
showColorForBoard White = "o"


type VertexSet = Set Vertex

type Vertex = (Coord, Coord)

type Coord = Int
type Coord1 = Int

type Boardsize = Int

type Score = Float



-- further accessors and minor helper functions

moveColor :: Move -> Color
moveColor (Move stone) = stoneColor stone
moveColor (Pass color) = color
moveColor (Resign color) = color

isStoneMove :: Move -> Bool
isStoneMove (Move _) = True
isStoneMove (Pass _) = False
isStoneMove (Resign _) = False






allVertices :: Boardsize -> [Vertex]
allVertices n =
    [(x, y) | y <- reverse [1 .. n], x <- [1 .. n]]

borderVertices :: Boardsize -> [Vertex]
borderVertices n =
    [(x, y) | x <- [0, n + 1], y <- [0 .. n + 1] ]
    ++ [(x, y) | x <- [1 .. n], y <- [0, n + 1] ]

{-# INLINE adjacentVertices #-}
adjacentVertices :: Vertex -> [Vertex]
adjacentVertices (x, y) =
    [(x,y-1),(x-1,y),(x+1,y),(x,y+1)]


-- {-# INLINE adjacentVerticesInBounds #-}
-- adjacentVerticesInBounds :: Boardsize -> Vertex -> [Vertex]
-- adjacentVerticesInBounds n (x, y) =
--     filter inBounds [(x,y-1),(x-1,y),(x+1,y),(x,y+1)]
--     where
--       inBounds :: Vertex -> Bool
--       inBounds (x', y') =
--           and [x' > 0, x' <= n, y' > 0, y' <= n]



diagonalVertices :: Vertex -> [Vertex]
diagonalVertices (x, y) =
    [(x+1,y+1),(x-1,y+1),(x+1,y-1),(x-1,y-1)]


otherColor :: Color -> Color
otherColor Black = White
otherColor White = Black



-- convert between GTPs A1 and (1, 1) vertex format

gtpShowMove :: Move -> String
gtpShowMove (Move (Stone vertex _color)) = gtpShowVertex vertex
gtpShowMove (Pass _color) = "pass"
gtpShowMove (Resign _color) = "resign"

gtpShowVertex :: Vertex -> String
gtpShowVertex (x, y) =
    [(xToLetter x)] ++ (show y)


xToLetter :: Coord -> Char
xToLetter n =
    if (n < 1) || (n > 25)
    then error "letterToX: n out of bounds"
    else
        if n <= 8
        then chr (fromIntegral $ n + 64)
        else chr (fromIntegral $ n + 65)

letterToX :: Char -> Coord
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
      n = (fromIntegral $ ord $ toUpper c) - 64

