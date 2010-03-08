{-# OPTIONS -O2 -Wall -Werror -Wwarn #-}

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
                        , Score
                        , gtpShowMove
                        , letterToX
                        ) where


import Data.Char (chr, ord, toUpper)

import Data.Tree.UCT.GameTree (UCTNode(..))


data Move = StoneMove Stone
          | Pass Color
          | Resign Color
            deriving (Eq)

instance Show Move where
    show (StoneMove (Stone ((x, y), color))) =
        (show color) ++ " " ++ [(xToLetter x)] ++ (show y)
    show (Pass _color) = "pass"
    show (Resign _color) = "resign"

instance UCTNode Move where
    -- we use estimated win rate of move as value for uct nodes
    initialMoveValue _ = 0.5
    updateBackpropagationValue _ v = 1 - v




newtype Stone = Stone (Vertex, Color)
    deriving (Show, Eq)


data Color = Black
           | White
             deriving (Eq, Ord, Enum)

instance Show Color where
    show Black = "b"
    show White = "w"

type Vertex = (Int, Int)

type Score = Float






gtpShowMove :: Move -> String
gtpShowMove (StoneMove (Stone ((x, y), _color))) =
    [(xToLetter x)] ++ (show y)
gtpShowMove (Pass _color) = "pass"
gtpShowMove (Resign _color) = "resign"

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

