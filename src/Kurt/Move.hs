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
                 ,genMoveMc
                 ,genMoveRand
                 ) where


import System.Random (split, randomR, StdGen)
import Data.List (sort, sortBy, (\\))
import Data.Map as M (Map, fromList, toList, insertWith, assocs)

import Data.Goban.Utils
import Data.Goban (GameState(..), updateGameState, score)
import Debug.Trace (trace)




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
            orderdMoves' = uct state color



uct :: GameState -> Color -> [(Vertex, (Float, Int))]
uct state color =
    reverse $ sortBy compareSnd $
            uct' initialMap (ourRandomGen state) (simulCount state)
    where
      uct' :: M.Map Vertex (Float, Int) -> StdGen -> Int -> [(Vertex, (Float, Int))]
      uct' m gen n
          | n < 1 = M.toList m
          | otherwise =
              uct' m' gen' (n - 1)
              where
                m' = M.insertWith combineProb a (result, 1) m

                result =
                    if (runScore < 0 && color == White) || (runScore > 0 && color == Black)
                    then 1
                    else 0

                runScore =
                    trace ("uct' random run score: " ++ show runScore')
                    runScore'
                    where
                      runScore' = runOneRandom state' color
                state' =
                    trace ("uct' runOneRandom for: " ++ show aMove)
                          updateGameState state aMove
                aMove = StoneMove (Stone (a, color))
                -- (a, gen') = pick' gen moves
                (a, gen') =
                    frequency gen $
                              map (\(v, (prob, _)) -> (prob, v)) $
                                  M.assocs m

      initialMap :: M.Map Vertex (Float, Int)
      initialMap = M.fromList $ initList

      initList =
          trace ("initList: " ++ show initList')
          initList'
          where
            initList' = zip moves $ take (length moves) (repeat (0.5, 1))

      moves = saneMoves state color

combineProb :: (Float, Int) -> (Float, Int) -> (Float, Int)
combineProb (prob, count) (oldProb, oldCount) =
    trace ("combineProb " ++ show (("old" ,oldProb, oldCount), ("new", prob, count), ("consolidated",prob', count')))
    (prob', count')
    where
      prob' = ((oldProb * (fromIntegral oldCount)) + prob) / (fromIntegral count')
      count' = oldCount + count


genMoveMc :: GameState -> Color -> Move
genMoveMc state color =
    if l'' == 0
    then Pass color
    else
        if bestScore > 10
        then Resign color
        else StoneMove (Stone (p, color))

    where
      p = head moves''
      l'' = length moves''

      moves'' =
          trace ("genMove, moveList'': " ++ show movesScored')
          movesScored'
          where
            movesScored' = map snd $ movesScored

      bestScore = fst $ head movesScored

      movesScored = sort $ map whatifScore moves'

      whatifScore v =
          (modifier * (runRandom 4 (updateGameState state move) color), v)
          where
            move = StoneMove (Stone (v, color))
            modifier = case color of
                         Black -> -1
                         White -> 1

      moves' = pickN 20 (ourRandomGen state) moves

      moves = saneMoves state color




runRandom :: Int -> GameState -> Color -> Score
runRandom n initState initColor =
    runRandom' n initState 0 initColor
    where
      runRandom' n' state totalScore color =
          if n' == 0
          then normalize totalScore
          else runRandom' (n' - 1) state' (totalScore + s) (otherColor color)
          where
            s = runOneRandom (state { ourRandomGen = g' }) color
            state' = state { ourRandomGen = g }
            (g, g') = split (ourRandomGen state)
      normalize tScore =
          sign * rt
          where
            rt = sqrt $ abs tScore
            sign = signum tScore


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


pickN :: (Eq a) => Int -> StdGen -> [a] -> [a]
pickN n g as =
    pickN' n as []
    where
      pickN' n' as' bs
             | n' == 0 = bs
             | as' == [] = bs
             | otherwise =
                 pickN' (n' - 1) (as' \\ [a]) (a : bs)
             where
               a = pick g as'

compareSnd :: (Ord t1) => (t, t1) -> (t2, t1) -> Ordering
compareSnd (_, a) (_, b) = compare a b
