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
                        ,territory
                        ,isSuicideVertex
                        ,isEyeLike
                        ,isDead
                        ,killedStones
                        ,adjacentFree
                        ,allVertices
                        ,verticesFromStones
                        ,groupOfStone
                        ,otherColor
                        ,xToLetter
                        ,letterToX
                        ) where

import Data.Char (chr, ord, toUpper)
import Data.List ((\\), nub)
-- import Debug.Trace (trace)


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
        filter inBounds [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]
        where
          inBounds (x', y') =
              and [x' > 0, x' <= boardsize, y' > 0, y' <= boardsize]
          boardsize = sizeOfGoban goban

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
              | color == Black = "X"
              | color == White = "O"
          showStone something = error ("showStone: unmatched " ++ show something)


data Move = StoneMove Stone
          | Pass Color
          | Resign Color
            deriving (Eq)

instance Show Move where
    show (StoneMove (Stone ((x, y), _color))) =
        [(xToLetter x)] ++ (show y)
    show (Pass _color) = "pass"
    show (Resign _color) = "resign"


newtype Stone = Stone (Vertex, Color)
    deriving (Show, Eq)

data Color = Black
           | White
             deriving (Show, Eq, Ord, Enum)

type Vertex = (Int, Int)

type Score = Float








territory :: (Goban a) => a -> Color -> Score
territory goban color =
    sum $ map (fromIntegral . length)
            $ filter f (emptyStrings goban)
    where
      f :: [Vertex] -> Bool
      f gs =
          all (((==) color) . stoneColor)
                  $ concatMap (adjacentStones goban) gs


emptyStrings :: (Goban a) => a -> [[Vertex]]
emptyStrings goban =
    emptyStrings' empties []
    where
      empties = (freeVertices goban)

      emptyStrings' [] gs = gs
      emptyStrings' (a : as) gs =
          emptyStrings' (as \\ ma) (ma : gs)
          where
            ma = maxEmptyString a

      maxEmptyString = maxString (adjacentVertices goban) isEmptyVertex

      isEmptyVertex v = (vertexToStone goban v) == Nothing


isSuicideVertex :: (Goban a) => a -> Color -> Vertex -> Bool
isSuicideVertex goban color v =
    isSuicide goban (Stone (v, color))

isEyeLike :: (Goban a) => a -> Color -> Vertex -> Bool
isEyeLike goban color v =
    (length vs == length sns)
    && isSuicide goban (Stone (v, (otherColor color)))
    where
      vs = adjacentVertices goban v
      sns = filter (\(Stone (_p', c')) -> color == c') ns
      ns = neighbourStones goban (Stone (v, color))


isSuicide :: Goban a => a -> Stone -> Bool
isSuicide goban stone =
    if isDead goban stone
    then isDead goban'' stone
    else False
    where
      goban'' = deleteStones goban' dead
      dead = killedStones goban' stone
      goban' = addStone goban stone


isDead :: Goban a => a -> Stone -> Bool
isDead goban stone =
    not . null $ deadStones goban stone


-- FIXME:
-- if several killed neighbouring stones are part of the same
-- group it will be found twice here
-- nub at the end works around for scoring
killedStones :: (Goban a) => a -> Stone -> [Stone]
killedStones goban stone@(Stone (_p, color)) =
    nub $ concatMap (deadStones goban) ns
    where
      ns = filter hasOtherColor $ neighbourStones goban stone
      hasOtherColor (Stone (_p', color')) =
          (otherColor color) == color'

deadStones :: Goban a => a -> Stone -> [Stone]
deadStones goban stone@(Stone (_p, color)) =
    anyInMaxStringAlive [stone] []
    where
      anyInMaxStringAlive [] gs =
          gs
      anyInMaxStringAlive (n@(Stone (p, _color)) : ns) gs =
          if null frees
          then
              anyInMaxStringAlive (ns ++ (((fgen n) \\ gs) \\ ns)) (n : gs)
          else
              []
          where
            frees = (adjacentFree goban p)

      genF stone' = neighbourStones goban stone'
      filterF (Stone (_p', color')) =
          color == color'
      fgen n =
          filter filterF $ genF n


-- liberties :: (Goban a) => a -> [Stone] -> Int
-- liberties goban groupStones =
--     length ls
--     where
--       ls = nub ls'
--       ls' =
--           concatMap
--           (adjacentFree goban)
--           (verticesFromStones groupStones)



groupOfStone :: (Goban a) => a -> Stone -> [Stone]
groupOfStone goban stone@(Stone (_p, color)) =
    maxString genF' filterF' stone
    where
      genF' stone' = neighbourStones goban stone'
      filterF' (Stone (_p', color')) =
          color == color'


neighbourStones :: (Goban a) => a -> Stone -> [Stone]
neighbourStones goban (Stone (p, _)) =
    adjacentStones goban p

adjacentStones :: (Goban a) => a -> Vertex -> [Stone]
adjacentStones goban p =
    concatMap toStoneList $ adjacentVertices goban p
    where
      toStoneList p' =
          case vertexToStone goban p' of
            Nothing -> []
            Just stone -> [stone]

adjacentFree :: (Goban a) => a -> Vertex -> [Vertex]
adjacentFree goban p =
    filter (((==) Nothing) . (vertexToStone goban)) $
           adjacentVertices goban p


nonEdgeVertices :: Int -> [Vertex]
nonEdgeVertices boardsize =
    [(x, y) | x <- [lower .. upper], y <- [lower .. upper]]
    where
      upper = boardsize - lower
      lower =
          if boardsize >= 9
          then 3
          else 2

allVertices :: Int -> [Vertex]
allVertices n =
    [(x, y) | y <- reverse [1 .. n], x <- [1 .. n]]

verticesFromStones :: [Stone] -> [Vertex]
verticesFromStones ss = map (\(Stone (p, _c)) -> p) ss



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




maxString :: (Eq a) => (a -> [a]) -> (a -> Bool) -> a -> [a]
maxString genF filterF p =
    maxString' [p] []
    where
      maxString' [] gs = gs
      maxString' (n : ns) gs =
          maxString' (ns ++ (((fgen n) \\ gs) \\ ns)) (n : gs)
      fgen n =
          filter filterF $ genF n

