{-# OPTIONS -O2 -Wall -Werror -Wwarn #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

{- |
   Module     : Data.Goban.Goban
   Copyright  : Copyright (C) 2010 Fabian Linzberger
   License    : GNU GPL, version 3 or above

   Maintainer : Fabian Linzberger <e@lefant.net>
   Stability  : experimental
   Portability: probably

General Types for Goban Implementation

-}

module Data.Goban.Goban ( -- STGoban(..)
                          Move(..)
                        , Color(..)
                        , Stone(..)
                        , Vertex
                        , Boardsize
                        , Score
                        , gtpShowMove
                        , gtpShowVertex
                        , letterToX
                        ) where


import Data.Char (chr, ord, toUpper)

import Data.Tree.UCT.GameTree (UCTNode(..))


data Move = StoneMove Stone
          | Pass Color
          | Resign Color
            deriving (Eq)

instance Show Move where
    show (StoneMove stone) = show stone
    show (Pass _color) = "pass"
    show (Resign _color) = "resign"

instance UCTNode Move where
    -- we use estimated win rate of move as value for uct nodes
    updateBackpropagationValue _ v = 1 - v




newtype Stone = Stone (Vertex, Color)
    deriving (Eq)

instance Show Stone where
    show (Stone (p, color)) =
        (show color) ++ " " ++ gtpShowVertex p

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
gtpShowMove (StoneMove (Stone (p, _color))) = gtpShowVertex p
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

