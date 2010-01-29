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
   Module     : Data.Tree.UCT
   Copyright  : Copyright (C) 2010 Fabian Linzberger
   License    : GNU GPL, version 3 or above

   Maintainer : Fabian Linzberger <e@lefant.net>
   Stability  : experimental
   Portability: probably


UCT tree search

-}

module Data.Tree.UCT (
                      genMove
                     ) where


import System.Random (split, randomR, StdGen)
import Data.List (deleteBy, sortBy, (\\))
import Data.Tree

import Data.Goban.Utils
import Data.Goban (GameState(..), updateGameState, score)
import Debug.Trace (trace)




type UctNode = ((UctProb, Vertex), GameState)

type UctProb = (Float, Int)

data Result = Win | Loss
            deriving (Show,Eq)



genMove :: GameState -> Color -> Move
genMove state color =
    case orderdMoves of
      ((p, (bestScore, _n)) : _) ->
          if bestScore < 0.1
          then Resign color
          else StoneMove (Stone (p, color))
      [] -> Pass color

    where
      orderdMoves =
          trace ("genMoves: " ++ show orderdMoves')
          orderdMoves'
          where
            orderdMoves' = reverse $ sortBy compareSnd weightedMoves

      weightedMoves = map moveFromTree forest
      forest =
          uctRunN 
          (ourRandomGen state)
          (simulCount state) $
                             initForest (state { toMove = color })


uctRunN :: StdGen -> Int -> Forest UctNode -> Forest UctNode
uctRunN gen n forest
        | n == 0 = forest
        | otherwise =
            uctRunN g (n - 1) $ fst (uctRun g' forest)
            where
              (g, g') = split gen

initTree :: UctProb -> Vertex -> GameState -> Tree UctNode
initTree prob vertex state =
    Node { rootLabel = ((prob, vertex), state),
           subForest = (initForest state) }

initForest :: GameState -> Forest UctNode
initForest state =
    case saneMoves state color of
      [] -> []
      moves -> map (treeFromMove state) moves
    where
      color = toMove state


treeFromMove :: GameState -> Vertex -> Tree UctNode
treeFromMove state vertex =
    Node { rootLabel =
               (((0.5, 1), vertex), state),
           subForest = [] }


uctRun :: StdGen -> Forest UctNode -> (Forest UctNode, Float)
uctRun _gen [] =
    ([], 0.0)
uctRun gen forest =
    -- trace ("uctRun returning")
    (forest', thisScore)
    where
      forest' = tree' : (deleteBy treeEq tree forest)

      (tree', thisScore) =
          case subForest tree of
            -- leaf node, run simulation
            -- return fresh subtree
            [] ->
                trace ("uctRun leaf "
                       ++ show (color,vertex,prob,runScore,runResult,(goban state')))
                (initTree prob vertex state', runScore)
                where
                  prob = updateProb oldProb runResult
                  runResult = scoreToResult runScore color
                  runScore = runOneRandom state color
                  state' = 
                      updateGameState state move
                  move = StoneMove (Stone (vertex, color))



            -- recurse, update win ratio with result
            childForest ->
                trace ("uctRun branch "
                       ++ show ((otherColor color),vertex,prob,childScore,childResult,(goban state)))
                (Node {
                   rootLabel =
                       ((prob, vertex), state),
                   subForest = childForest' },
                 childScore)
                where
                  prob = updateProb oldProb childResult
                  childResult = scoreToResult childScore (otherColor color)
                  (childForest', childScore) = uctRun gen' childForest

      color = toMove state
      ((oldProb, vertex), state) = rootLabel tree

      -- randomly pick one respecting weights
      (tree, gen') = frequency gen weightForest

      weightForest = map weightTree forest



treeEq :: Tree UctNode -> Tree UctNode -> Bool
treeEq a b =
    va == vb
    where
      ((_, va), _) = rootLabel a
      ((_, vb), _) = rootLabel b

updateProb :: UctProb -> Result -> UctProb
updateProb (oldProb, oldCount) result =
    (prob, count)
    where
      prob =
          case result of
            Win ->
                ((oldProb * (fromIntegral oldCount)) + 1) / (fromIntegral count)
            Loss ->
                (oldProb * (fromIntegral oldCount)) / (fromIntegral count)
      count = oldCount + 1


scoreToResult :: Float -> Color -> Result
scoreToResult aScore color =
    if (aScore < 0 && color == White) ||
           (aScore > 0 && color == Black)
    then Win
    else Loss

-- otherResult :: Result -> Result
-- otherResult Win = Loss
-- otherResult Loss = Win

moveFromTree :: Tree UctNode -> (Vertex, UctProb)
moveFromTree tree =
    (vertex, prob)
    where
      ((prob, vertex), _state) = rootLabel tree


weightTree :: Tree UctNode -> (Float, Tree UctNode)
weightTree tree =
    (prob, tree)
    where
      (((prob, _simuls), _vertex), _state) = rootLabel tree




runOneRandom :: GameState -> Color -> Score
runOneRandom initState color =
    run initState
    where
      run state =
        case move of
          (Pass _) ->
              case move' of
                (Pass _) ->
                    score state''
                (StoneMove _) ->
                    run state''
                (Resign _) ->
                    error "runOneRandom encountered Resign"
              where
                move' = genMoveRand (state' { ourRandomGen = gg }) color
                state'' =
                    updateGameState state' { ourRandomGen = gg' } move'
                (gg, gg') = split g'

          (StoneMove _) ->
              run state'
          (Resign _) ->
              error "runOneRandom encountered Resign"

        where
          move = genMoveRand (state { ourRandomGen = g }) color
          state' =
              updateGameState state { ourRandomGen = g' } move
          (g, g') = split (ourRandomGen state)




genMoveRand :: GameState -> Color -> Move
genMoveRand state color =
    if length moves == 0
    then Pass color
    else StoneMove (Stone (p, color))

    where
      p = pick (ourRandomGen state) moves
      moves = saneMoves state color




saneMoves :: GameState -> Color -> [Vertex]
saneMoves state color =
    filter (not . (isEyeLike g color)) $
           filter (not . (isSuicideVertex g color)) $
                      (freeVertices g) \\ (koBlocked state)
    where
      g = goban state


pick :: StdGen -> [a] -> a
pick g as =
    as !! i
    where
      (i, _g) = randomR (0, ((length as) - 1)) g

-- pick' :: StdGen -> [a] -> (a, StdGen)
-- pick' g as =
--     (as !! i, g')
--     where
--       (i, g') = randomR (0, ((length as) - 1)) g

frequency :: StdGen -> [(Float, a)] -> (a, StdGen)
frequency _g [] = error "frequency used with empty list"
frequency g as =
    (pickF i as', g')
    where
      tot = sum (map fst as')
      as' = map fstToInt as
      fstToInt (a, b) =
          ((round (a * 1000) :: Int), b)

      (i, g') = randomR (0, tot) g

      pickF n ((k,x):xs)
          | n <= k    = x
          | otherwise = pickF (n-k) xs
      pickF _ _  = error "pick used with empty list"


-- pickN :: (Eq a) => Int -> StdGen -> [a] -> [a]
-- pickN n g as =
--     pickN' n as []
--     where
--       pickN' n' as' bs
--              | n' == 0 = bs
--              | as' == [] = bs
--              | otherwise =
--                  pickN' (n' - 1) (as' \\ [a]) (a : bs)
--              where
--                a = pick g as'


compareSnd :: (Ord t1) => (t, t1) -> (t2, t1) -> Ordering
compareSnd (_, a) (_, b) = compare a b
