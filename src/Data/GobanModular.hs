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

module Data.GobanModular (
                          GameState(..)
                         ,defaultGameState
                         ,updateGameState
                         ,score
                         ,adjacentVertices
                         ) where

import Data.List
import Data.Maybe
import System.Random (StdGen)

import Data.Goban.Utils
import Data.Goban.StoneList

data GameState = GameState {
      goban           :: Goban
     ,komi            :: Score
     ,toMove          :: Color
     ,koBlocked       :: [Vertex]
     ,moveHistory     :: [Move]
     ,blackPrisoners  :: Score
     ,whitePrisoners  :: Score
     ,ourRandomGen    :: StdGen
    } deriving (Show)

defaultGameState :: StdGen -> GameState
defaultGameState g = GameState {
                     komi = 0
                    ,toMove = Black
                    ,koBlocked = []
                    ,moveHistory = []
                    ,blackPrisoners = 0
                    ,whitePrisoners = 0
                    ,ourRandomGen = g
                    ,goban = StoneListGoban
                   }
--                     ,stones = []
--                     ,boardsize = 1




updateGameState :: GameState -> Move -> GameState
updateGameState state move =
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
    -- trace ("score: "
    --        ++ show (
    --                 ("s: ", s),
    --                 ("b: ", b),
    --                 ("w: ", w),
    --                 ("bt: ", bt),
    --                 ("bp: ", bp),
    --                 ("wt: ", wt),
    --                 ("wp: ", wp),
    --                 ("k: ", k)
    --                ))
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
