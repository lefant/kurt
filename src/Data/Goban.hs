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
   Module     : Data.Goban
   Copyright  : Copyright (C) 2010 Fabian Linzberger
   License    : GNU GPL, version 3 or above

   Maintainer : Fabian Linzberger <e@lefant.net>
   Stability  : experimental
   Portability: probably

Goban Implementation

-}

module Data.Goban (
                   GameState(..)
                  ,defaultGameState
                  ,Move(..)
                  ,Color(..)
                  ,Stone(..)
                  ,Vertex
                  ,Score
                  ,updateGameState
                  ,freeVertices
                  ,isSuicide
                  ) where

import Kurt.Utils (xToLetter)

import Data.List
import Data.Maybe



data GameState = GameState {
      komi            :: Score
     ,boardsize       :: Int
     ,toMove          :: Color
     ,stones          :: [Stone]
     ,moveHistory     :: [Move]
     ,blackPrisoners  :: Score
     ,whitePrisoners  :: Score
    } deriving (Show, Eq)

defaultGameState :: GameState
defaultGameState = GameState {
                     komi = 0
                    ,boardsize = 1
                    ,toMove = Black
                    ,stones = []
                    ,moveHistory = []
                    ,blackPrisoners = 0
                    ,whitePrisoners = 0
                   }


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



updateGameState :: GameState -> Move -> GameState
updateGameState state move =
    if (moveColor move) /= currentToMove
    then error "updateGameState: move by wrong color received"
    else
        case move of
          StoneMove stone ->
              state {
                    toMove = toMove'
                   ,stones = stones'''
                   ,moveHistory = (moveHistory state) ++ [move]
                   ,blackPrisoners = blackPrisoners'
                   ,whitePrisoners = whitePrisoners'
                  }
              where
                dead = deadStones bsize stone stones'
                stones' = (stone : (stones state))
                stones'' = stones' \\ dead
                stones''' = stones'' \\ dead'
                dead' =
                    if isDead bsize stone stones''
                    then (groupOfStone bsize stone stones'')
                    else []
                bsize = (boardsize state)
                blackPrisoners' =
                    (blackPrisoners state)
                    + (fromIntegral $ length bDead)
                whitePrisoners' =
                    (whitePrisoners state)
                    + (fromIntegral $ length wDead)
                (bDead, wDead) = partition
                                 (\(Stone (_, c)) -> c == Black)
                                 (dead ++ dead')

          Pass _color ->
              state {
                    toMove = toMove'
                   ,moveHistory = (moveHistory state) ++ [move]
                  }
    where
      currentToMove = toMove state
      toMove' = case currentToMove of
                  Black -> White
                  White -> Black



isSuicide :: Int -> Stone -> [Stone] -> Bool
isSuicide bsize stone allStones =
    isDead bsize stone stones''
    where
      dead = deadStones bsize stone stones'
      stones' = (stone : allStones)
      stones'' = stones' \\ dead

isDead :: Int -> Stone -> [Stone] -> Bool
isDead bsize stone allStones =
    liberties bsize (groupOfStone bsize stone allStones) allStones == 0

deadStones :: Int -> Stone -> [Stone] -> [Stone]
deadStones bsize (Stone (p, _)) ss =
    concatMap dead_stones' ns
    where
      ns = neighbourStones bsize p ss
      dead_stones' n =
          if liberties bsize groupStones ss == 0
          then groupStones
          else []
          where
            groupStones = groupOfStone bsize n ss
            


liberties :: Int -> [Stone] -> [Stone] -> Int
liberties bsize groupStones allStones =
    length ls
    where
      ls = nub ls'
      ls' = concatMap freeAdjacentVertices groupVertices

      groupVertices = (verticesFromStones groupStones)

      freeAdjacentVertices :: Vertex -> [Vertex]
      freeAdjacentVertices p =
          (adjacentVertices bsize p) `intersect` free

      free = (freeVertices bsize allStones)


-- groupOfVertex :: Int -> Vertex -> [Stone] -> [Stone]
-- groupOfVertex bsize p ss =
--     case toStone p ss of
--       Nothing -> []
--       Just stone -> groupOfStone bsize stone ss

groupOfStone :: Int -> Stone -> [Stone] -> [Stone]
groupOfStone bsize s ss =
    groupOfStone' (neighbourStonesSameColor bsize s ss) [s]
    where
      groupOfStone' [] gs = gs
      groupOfStone' (n : ns) gs =
          groupOfStone' (ns ++ (((neighbourStonesSameColor bsize n ss) \\ gs) \\ ns)) (n : gs)


neighbourStonesSameColor :: Int -> Stone -> [Stone] -> [Stone]
neighbourStonesSameColor bsize (Stone (p, color)) ss =
    filter sameColor $ neighbourStones bsize p ss
    where
      sameColor (Stone (_p', color')) =
          color == color'

neighbourStones :: Int -> Vertex -> [Stone] -> [Stone]
neighbourStones bsize p ss =
    concatMap toStoneList $ adjacentVertices bsize p
    where
      toStoneList p' =
          case toStone p' ss of
            Nothing -> []
            Just stone -> [stone]

-- isStone :: Vertex -> [Stone] -> Bool
-- isStone p ss =
--     case toStone p ss of
--       Nothing -> False
--       Just _ -> True

toStone :: Vertex -> [Stone] -> Maybe Stone
toStone p ss =
    case lookup p ss' of
      Nothing -> Nothing
      Just color -> Just $ Stone (p, color)
    where
      ss' = map (\(Stone s) -> s) ss

-- vertexFromStone :: Stone -> Vertex
-- vertexFromStone (Stone (p, _color)) = p

adjacentVertices :: Int -> Vertex -> [Vertex]
adjacentVertices bsize (x, y) =
    filter inBounds [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]
    where
      inBounds (x', y') =
          and [x' > 0, x' <= bsize, y' > 0, y' <= bsize]


freeVertices :: Int -> [Stone] -> [Vertex]
freeVertices n ss =
    (allVertices n) \\ (verticesFromStones ss)

verticesFromStones :: [Stone] -> [Vertex]
verticesFromStones ss = map (\(Stone (p, _c)) -> p) ss


allVertices :: Int -> [Vertex]
allVertices n =
    [(x, y) | x <- [1 .. n], y <- [1 .. n]]


moveColor :: Move -> Color
moveColor (StoneMove (Stone (_vertex, color))) = color
moveColor (Pass color) = color
