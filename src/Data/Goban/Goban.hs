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

module Data.Goban.Goban ( Goban(..)
                        , Move(..)
                        , Color(..)
                        , Stone(..)
                        , Vertex
                        , Score
                        , gtpShowMove
                        , allVertices
                        , letterToX
                        ) where

import Data.Char (chr, ord, toUpper)
-- import Debug.Trace (trace)

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

gtpShowMove :: Move -> String
gtpShowMove (StoneMove (Stone ((x, y), _color))) =
    [(xToLetter x)] ++ (show y)
gtpShowMove (Pass _color) = "pass"
gtpShowMove (Resign _color) = "resign"



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









class Goban a where
    addStone :: a -> Stone -> a
    deleteStones :: a -> [Stone] -> a
    freeVertices :: a -> [Vertex]
    vertexToStone :: a -> Vertex -> Maybe Stone
    sizeOfGoban :: a -> Int
    newGoban :: Int -> a

    clearGoban :: a -> a
    clearGoban goban = newGoban $ sizeOfGoban goban

    adjacentVertices :: a -> Vertex -> [Vertex]
    adjacentVertices goban (x, y) =
        filter
        (inBounds (sizeOfGoban goban))
        [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]

    diagonalVertices :: a -> Vertex -> [Vertex]
    diagonalVertices goban (x, y) =
        filter
        (inBounds (sizeOfGoban goban))
        [(x+1,y+1),(x-1,y+1),(x+1,y-1),(x-1,y-1)]


    freeNonEdgeVertices :: a -> [Vertex]
    freeNonEdgeVertices goban =
        filter (((==) Nothing) . (vertexToStone goban)) $
               nonEdgeVertices (sizeOfGoban goban)

    showboard :: a -> String
    showboard goban =
        show' $ map (vertexToStone goban) $ allVertices (sizeOfGoban goban)
        where
          show' [] = ""
          show' ls' = concatMap showStone left ++ "\n" ++ show' right
              where
                (left, right) = splitAt n ls'
          n = sizeOfGoban goban
          showStone Nothing = "."
          showStone (Just (Stone (_, color)))
              | color == Black = "x"
              | color == White = "o"
          showStone something = error ("showStone: unmatched " ++ show something)







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



inBounds :: Int -> Vertex -> Bool
inBounds boardsize (x, y) =
    and [x > 0, x <= boardsize, y > 0, y <= boardsize]





nonEdgeVertices :: Int -> [Vertex]
nonEdgeVertices boardsize =
    [(x, y) | x <- [lower .. upper], y <- [lower .. upper]]
    where
      upper = boardsize - lower + 1
      lower =
          if boardsize >= 9
          then 3
          else 2


allVertices :: Int -> [Vertex]
allVertices n =
    [(x, y) | y <- reverse [1 .. n], x <- [1 .. n]]

