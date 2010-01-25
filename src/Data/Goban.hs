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
                  ,otherColor
                  ,adjacentVertices
                  ,neighbourStones
                  ,score
                  ) where

import Kurt.Utils (xToLetter)

import Data.List
import Data.Maybe
import Debug.Trace (trace)


data GameState = GameState {
      komi            :: Score
     ,boardsize       :: Int
     ,toMove          :: Color
     ,stones          :: [Stone]
     ,koBlocked       :: [Vertex]
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
                    ,koBlocked = []
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
    if (moveColor move) /= (toMove state)
    then error "updateGameState: move by wrong color received"
    else
        case move of
          StoneMove stone@(Stone (p, _)) ->
              if p `elem` (koBlocked state)
              then error "updateGameState: move in ko violation"
              else
                  -- trace ("updateGameState: "
                  --        ++ show (
                  --                 (" move ", move)
                  --                 ,(" bp: ", blackPrisoners')
                  --                 ,(" wp: ", whitePrisoners')
                  --                 ,(" dead: ", dead)
                  --                 ,(" dead': ", dead')
                  --                 ,(" bdead': ", bDead)
                  --                 ,(" wdead': ", wDead)
                  --                ))
                  state {
                       toMove = otherColor (toMove state)
                      ,stones = stones'''
                      ,moveHistory = (moveHistory state) ++ [move]
                      ,blackPrisoners = blackPrisoners'
                      ,whitePrisoners = whitePrisoners'
                      ,koBlocked = koBlocked'
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
                koBlocked' = if (length dead) == 1
                             then verticesFromStones dead
                             else []

          Pass _color ->
              state {
                    toMove = otherColor (toMove state)
                   ,moveHistory = (moveHistory state) ++ [move]
                   ,koBlocked = []
                  }


score :: GameState -> Score
score state =
    trace ("score: "
           ++ show (
                    ("s: ", s),
                    ("b: ", b),
                    ("w: ", w),
                    ("bt: ", bt),
                    ("bp: ", bp),
                    ("wt: ", wt),
                    ("wp: ", wp),
                    ("k: ", k)
                   ))
    s
    where
      s = b - w
      b = bt + bp
      w = wt + wp + k
      bt = colorTerritory Black
      bp = whitePrisoners state
      wt = colorTerritory White
      wp = blackPrisoners state
      k = (komi state)

      colorTerritory =
          territory (boardsize state) (stones state)



isSuicide :: Int -> Stone -> [Stone] -> Bool
isSuicide bsize stone allStones =
    isDead bsize stone stones''
    where
      dead = deadStones bsize stone stones'
      stones' = (stone : allStones)
      stones'' = stones' \\ dead

isDead :: Int -> Stone -> [Stone] -> Bool
isDead bsize stone allStones =
    -- trace ("isDead called with: " ++ (show stone) ++ " liberties: " ++ (show libertyCount))
    libertyCount == 0
    where
      libertyCount = liberties bsize (groupOfStone bsize stone allStones) allStones


-- FIXME:
-- if several killed neighbouring stones are part of the same
-- group it will be found twice here
-- nub at the end works around for scoring
deadStones :: Int -> Stone -> [Stone] -> [Stone]
deadStones bsize stone@(Stone (_p, color)) allStones =
    nub $ concatMap dead_stones' ns
    where
      dead_stones' n =
          if liberties bsize groupStones allStones == 0
          then groupStones
          else []
          where
            groupStones = groupOfStone bsize n allStones

      ns = filter hasOtherColor $ neighbourStones bsize allStones stone

      hasOtherColor (Stone (_p', color')) =
          (otherColor color) == color'



liberties :: Int -> [Stone] -> [Stone] -> Int
liberties bsize groupStones allStones =
    length ls
    where
      ls = nub ls'
      ls' = concatMap (adjacentFree bsize allStones) groupVertices
      groupVertices = (verticesFromStones groupStones)


territory :: Int -> [Stone] -> Color -> Score
territory bsize allStones color =
    sum $ map (fromIntegral . length)
            $ filter f (emptyStrings bsize allStones)
    where
      f :: [Vertex] -> Bool
      f gs =
          all (((==) color) . stoneColor)
                  $ concatMap (adjacentStones bsize allStones) gs


emptyStrings :: Int -> [Stone] -> [[Vertex]]
emptyStrings bsize allStones =
    emptyStrings' empties []
    where
      empties = (freeVertices bsize allStones)

      emptyStrings' [] gs = gs
      emptyStrings' (a : as) gs =
          emptyStrings' (as \\ ma) (ma : gs)
          where
            ma = maxEmptyString a

      maxEmptyString = maxString (adjacentVertices bsize) isEmptyVertex

      isEmptyVertex v = (toStone allStones v) == Nothing



maxString :: (Eq a) => (a -> [a]) -> (a -> Bool) -> a -> [a]
maxString genF filterF p =
    maxString' [p] []
    where
      maxString' [] gs = gs
      maxString' (n : ns) gs =
          maxString' (ns ++ (((fgen n) \\ gs) \\ ns)) (n : gs)
      fgen n =
          filter filterF $ genF n


groupOfStone :: Int -> Stone -> [Stone] -> [Stone]
groupOfStone bsize stone@(Stone (_p, color)) allStone =
    maxString genF' filterF' stone
    where
      genF' stone' = neighbourStones bsize allStone stone'
      filterF' (Stone (_p', color')) =
          color == color'

neighbourStones :: Int -> [Stone] -> Stone -> [Stone]
neighbourStones bsize allStones (Stone (p, _)) =
    adjacentStones bsize allStones p

adjacentStones :: Int -> [Stone] -> Vertex -> [Stone]
adjacentStones bsize allStones p =
    concatMap toStoneList $ adjacentVertices bsize p
    where
      toStoneList p' =
          case toStone allStones p' of
            Nothing -> []
            Just stone -> [stone]

adjacentFree :: Int -> [Stone] -> Vertex -> [Vertex]
adjacentFree bsize allStones p =
    filter (((==) Nothing) . (toStone allStones)) $ adjacentVertices bsize p

adjacentVertices :: Int -> Vertex -> [Vertex]
adjacentVertices bsize (x, y) =
    filter inBounds [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]
    where
      inBounds (x', y') =
          and [x' > 0, x' <= bsize, y' > 0, y' <= bsize]


freeVertices :: Int -> [Stone] -> [Vertex]
freeVertices n ss =
    (allVertices n) \\ (verticesFromStones ss)


allVertices :: Int -> [Vertex]
allVertices n =
    [(x, y) | x <- [1 .. n], y <- [1 .. n]]



verticesFromStones :: [Stone] -> [Vertex]
verticesFromStones ss = map (\(Stone (p, _c)) -> p) ss

toStone :: [Stone] -> Vertex -> Maybe Stone
toStone allStones p =
    case lookup p allStones' of
      Nothing -> Nothing
      Just color -> Just $ Stone (p, color)
    where
      allStones' = map (\(Stone s) -> s) allStones


moveColor :: Move -> Color
moveColor (StoneMove stone) = stoneColor stone
moveColor (Pass color) = color

stoneColor :: Stone -> Color
stoneColor (Stone (_vertex, color)) = color

otherColor :: Color -> Color
otherColor Black = White
otherColor White = Black



-- maxVertexString :: Int -> (Vertex -> Bool) -> Vertex -> [Vertex]
-- maxVertexString bsize f p =
--     maxString' [p] []
--     where
--       maxString' [] gs = gs
--       maxString' (n : ns) gs =
--           maxString' (ns ++ (((helper n) \\ gs) \\ ns)) (n : gs)
--       helper n =
--           filter f $ adjacentVertices bsize n



-- groupOfVertex :: Int -> Vertex -> [Stone] -> [Stone]
-- groupOfVertex bsize p ss =
--     case toStone p ss of
--       Nothing -> []
--       Just stone -> groupOfStone bsize stone ss

-- groupOfStone' :: Int -> Stone -> [Stone] -> [Stone]
-- groupOfStone' bsize s ss =
--     groupOfStone' [s] []
--     where
--       groupOfStone' [] gs = gs
--       groupOfStone' (n : ns) gs =
--           groupOfStone' (ns ++ (((neighbourStonesSameColor bsize n ss) \\ gs) \\ ns)) (n : gs)


-- neighbourStonesSameColor :: Int -> Stone -> [Stone] -> [Stone]
-- neighbourStonesSameColor bsize (Stone (p, color)) ss =
--     filter sameColor $ neighbourStones bsize p ss
--     where
--       sameColor (Stone (_p', color')) =
--           color == color'



-- isStone :: Vertex -> [Stone] -> Bool
-- isStone p ss =
--     case toStone p ss of
--       Nothing -> False
--       Just _ -> True


-- vertexFromStone :: Stone -> Vertex
-- vertexFromStone (Stone (p, _color)) = p
