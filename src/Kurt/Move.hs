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
   Module     : Kurt.Move
   Copyright  : Copyright (C) 2010 Fabian Linzberger
   License    : GNU GPL, version 3 or above

   Maintainer : Fabian Linzberger <e@lefant.net>
   Stability  : experimental
   Portability: probably


Move generator logic

-}

module Kurt.Move (
                  genMove
                 ,genMoveRand
                 ) where


-- import System.Random (split, randomR, StdGen)
import System.Random (RandomGen)
import Control.Monad.Random (Rand, getRandomR)
import Data.List ((\\))
-- import Data.Map as M (Map, fromList, toList, insertWith, assocs)
import Debug.Trace (trace)


import Data.Goban.Utils
import Data.Goban (GameState(..), updateGameState, score)
import Data.Tree.UCT


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
              if s < 0
              then 1.0
              else 0.0
          White ->
              if s > 0
              then 1.0
              else 0.0
        where
          s = score state

    randomEvalOnce state = do
        s <- runOneRandom state color
        return $ case color of
          Black -> 
              if s < 0
              then 1.0
              else 0.0
          White ->
              if s > 0
              then 1.0
              else 0.0
          where
            color = toMove state


    children state =
        case saneMoves state color of
          [] -> [updateGameState state (Pass color)]
          vs -> map (\v ->
                         updateGameState state (StoneMove (Stone (v, color)))) vs
        where
          color = toMove state





genMove :: (RandomGen g) => GameState -> Color -> g -> Move
genMove state color rGen =
    if null (saneMoves state color)
    then Pass color
    else
        if (winningProb bestMove) < 0.1
           then Resign color
           else last $ moveHistory $ nodeState bestMove
    where
      bestMove =
          trace ("genMoves: " ++ show pv)
          head pv
      pv = uct state (simulCount state) rGen


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
