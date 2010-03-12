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
                        , Stone(..)
                        , Color(..)
                        , Vertex
                        , Boardsize
                        , Score
                        , gtpShowMove
                        , gtpShowVertex
                        , letterToX
                        ) where


import Data.Char (chr, ord, toUpper)

import Data.Tree.UCT.GameTree (UCTNode(..))



data Move = Move Stone
          | Pass Color
          | Resign Color
            deriving (Eq, Ord)

instance Show Move where
    show (Move stone) = show stone
    show (Pass color) = show color ++ " pass"
    show (Resign color) = show color ++ " resign"

instance UCTNode Move where
    -- we use estimated win rate of move as value for uct nodes
    updateBackpropagationValue _ v = 1 - v



data Stone = Stone Vertex Color
           deriving (Eq, Ord)

instance Show Stone where
    show (Stone vertex color) =
        (show color) ++ " " ++ gtpShowVertex vertex



-- this should be some kind of enum instance
-- so we can do toEnum / fromEnum instead of stateToWord in Goban implementations
data VertexState = Colored Color | Empty | Border
                 deriving (Eq)

instance Show VertexState where
    show (Colored color) = show color
    show Empty = "."
    show Border = " "



data Color = Black
           | White
             deriving (Eq, Ord, Enum)

instance Show Color where
    show Black = "b"
    show White = "w"


type Vertex = (Coord, Coord)

type Coord = Int

type Boardsize = Int

type Score = Float





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

