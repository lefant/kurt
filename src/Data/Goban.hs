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
                  ,defaultGoban
                  ,updateGameState
                  ,score
                  ,saneMoves
                  ) where

import Data.List (partition)
import System.Random (StdGen, RandomGen)
import Control.Monad.Random (Rand, getRandomR)
import Data.List ((\\))
import Debug.Trace (trace)

import Data.Goban.Utils
-- import Data.Goban.StoneList (StoneListGoban)
-- import Data.Goban.Array (ArrayGoban)
import Data.Goban.DataMap (DataMapGoban)
import Data.Tree.UCT


data GameState = GameState {
      goban           :: DataMapGoban
     ,komi            :: Score
     ,koBlocked       :: [Vertex]
     ,moveHistory     :: [Move]
     ,blackPrisoners  :: Score
     ,whitePrisoners  :: Score
     ,ourRandomGen    :: StdGen
     ,simulCount      :: Int
    }

instance Show GameState where
    show state =
        case moveHistory state of
          [] -> ""
          moves -> m
            where
              m = case last moves of
               (StoneMove (Stone ((x, y), color))) ->
                   c ++ [(xToLetter x)] ++ (show y)
                   where
                     c = case color of
                           Black -> "b "
                           White -> "w "
               (Pass _color) -> "pass"
               (Resign _color) -> "resign"


instance UctNode GameState where
    isTerminalNode state =
        case reverse $ moveHistory state of
          [] -> False
          [_] -> False
          ((Pass _) : (Pass _) : _) -> True
          _ -> False

    finalResult state =
        case toMove state of
          Black ->
              if s > 0
              then 1.0
              else 0.0
          White ->
              if s < 0
              then 1.0
              else 0.0
        where
          s = score state

    -- randomEvalOnce _state =
    --     getRandomR (0, 1)
    randomEvalOnce state = do
        s <- runOneRandom state color
        return $ trace ("randomEvalOnce: " ++ (show (show color, show s, show (res s)))) (res s)
          where
            color = toMove state
            res :: Score -> Float
            res s =
                case color of
                  Black ->
                      if s < 0
                      then 1.0
                      else 0.0
                  White ->
                      if s > 0
                      then 1.0
                      else 0.0



    children state =
        case saneMoves state color of
          [] -> [updateGameState state (Pass color)]
          vs -> map (\v ->
                         updateGameState state (StoneMove (Stone (v, color)))) vs
        where
          color = toMove state




defaultGameState :: StdGen -> GameState
defaultGameState g = GameState {
                     goban = (defaultGoban 1)
                    ,komi = 0
                    ,koBlocked = []
                    ,moveHistory = []
                    ,blackPrisoners = 0
                    ,whitePrisoners = 0
                    ,ourRandomGen = g
                    ,simulCount = 100
                   }

thisMoveColor :: GameState -> Color
thisMoveColor state =
    case moveHistory state of
      [] -> White
          -- error "thisMoveColor called when moveHistory still empty"
      moves ->
          case last moves of
            (StoneMove (Stone (_, color))) -> color
            (Pass color) -> color
            (Resign color) -> color

toMove :: GameState -> Color
toMove state =
    otherColor $ thisMoveColor state


defaultGoban :: (Goban a) => Int -> a
defaultGoban = newGoban



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
                       goban = goban''
                      ,moveHistory = (moveHistory state) ++ [move]
                      ,blackPrisoners = blackPrisoners'
                      ,whitePrisoners = whitePrisoners'
                      ,koBlocked = koBlocked'
              }
          where
            dead = deadStones goban' stone
            goban' = addStone (goban state) stone
            goban'' = deleteStones goban' dead
            -- goban''' = deleteStones goban'' dead'
            -- dead' =
            --     if isDead goban'' stone
            --     then (groupOfStone goban'' stone)
            --     else []
            blackPrisoners' =
                (blackPrisoners state)
                + (fromIntegral $ length bDead)
            whitePrisoners' =
                (whitePrisoners state)
                + (fromIntegral $ length wDead)
            (bDead, wDead) = partition
                             (\(Stone (_, c)) -> c == Black)
                             -- (dead ++ dead')
                             dead

            koBlocked' =
                case dead of
                  [koStone@(Stone (v,_))] ->
                      if [stone] == (deadStones goban' koStone)
                      then [v]
                      else []
                  _ -> []


      Pass _color ->
          state {
                moveHistory = (moveHistory state) ++ [move]
               ,koBlocked = []
              }

      Resign _color ->
          state {
                moveHistory = (moveHistory state) ++ [move]
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
          territory (goban state)



runOneRandom :: (RandomGen g) => GameState -> Color -> Rand g Score
runOneRandom initState color =
    run initState
    where
      run state = do
        move <- genMoveRand state color
        state' <- return $ updateGameState state move
        case move of
          (Pass _) -> do
                    move' <- genMoveRand state' color
                    state'' <- return $ updateGameState state' move'
                    case move' of
                      (Pass _) ->
                          return $ score state''
                      (StoneMove _) ->
                          run state''
                      (Resign _) ->
                          error "runOneRandom encountered Resign"
          (StoneMove _) ->
              run $ updateGameState state move
          (Resign _) ->
              error "runOneRandom encountered Resign"



genMoveRand :: (RandomGen g) => GameState -> Color -> Rand g Move
genMoveRand state color =
    if length moves == 0
    then return $ Pass color
    else (do
           p <- pick moves
           return $ StoneMove (Stone (p, color)))
    where
      moves = saneMoves state color


saneMoves :: GameState -> Color -> [Vertex]
saneMoves state color =
    filter (not . (isEyeLike g color)) $
           filter (not . (isSuicideVertex g color)) $
                      (freeVertices g) \\ (koBlocked state)
    where
      g = goban state



pick :: (RandomGen g) => [a] -> Rand g a
pick as = do
  i <- getRandomR (0, ((length as) - 1))
  return $ as !! i

