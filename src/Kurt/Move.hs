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

import System.Random
-- import System.Random.Shuffle (shuffle')
import Data.List

import Data.Goban
import Debug.Trace (trace)




genMove :: GameState -> Move
genMove state =
    if l'' == 0
    then Pass color
    else StoneMove (Stone (p, color))

    where
      p = head moveList''''
      l'' = length moveList''''

      -- moveList''' = drop ((length moveList'') `div` 2) moveList''
      moveList'''' =
          trace ("genMove, moveList'': " ++ show resMoveListScored)
          resMoveList'''
          where
            resMoveList''' = map snd $ resMoveListScored

            resMoveListScored = sort $ map whatifScore moveList'''

            whatifScore v =
                (modifier * (runRandom 1 (updateGameState state move)), v)
                where
                  move = StoneMove (Stone (v, color))


            modifier = case color of
                         Black -> -1
                         White -> 1

      moveList''' = pickN 10 (ourRandomGen state) moveList''

      moveList'' =
          -- trace ("genMove, moveList'': " ++ show resMoveList'')
          resMoveList''
          where
            resMoveList'' = filter (not . isEyeLike) moveList'
      moveList' =
          -- trace ("genMove, moveList': " ++ show resMoveList')
          resMoveList'
          where
            resMoveList' = filter (not . isSuicide') moveList
      moveList =
          -- trace ("genMove, moveList: " ++ show resMoveList)
          resMoveList
          where
            resMoveList = (freeVertices bsize allStones)
                          \\ (koBlocked state)

      bsize = (boardsize state)
      allStones = (stones state)
      color = (toMove state)

      isSuicide' :: Vertex -> Bool
      isSuicide' v = isSuicide bsize (Stone (v, color)) allStones

      isEyeLike :: Vertex -> Bool
      isEyeLike v =
          (length vs == length sns)
          && isSuicide bsize (Stone (v, (otherColor color))) allStones
          where
            vs = adjacentVertices bsize v
            sns = filter (\(Stone (_p', c')) -> color == c') ns
            ns = neighbourStones bsize allStones (Stone (v, color))


runRandom :: Int -> GameState -> Score
runRandom n initState =
    (runRandom' n initState 0) / (fromIntegral n)
    where
      runRandom' n' state totalScore =
          if n' == 0
          then totalScore
          else runRandom' (n' - 1) state' (totalScore + s)
          where
            s = runOneRandom state { ourRandomGen = g' }
            state' = state { ourRandomGen = g }
            (g, g') = split (ourRandomGen state)

runOneRandom :: GameState -> Score
runOneRandom initState =
    -- signum $ run initState
    id $ run initState
    where
      run state =
        case move of
          (Pass _) ->
              case move' of
                (Pass _) ->
                    score state''
                (StoneMove _) ->
                    run state''
              where
                move' = genMoveRand state' { ourRandomGen = gg }
                state'' =
                    updateGameState state' { ourRandomGen = gg' } move'
                (gg, gg') = split g'

          (StoneMove _) ->
              run state'

        where
          move = genMoveRand state { ourRandomGen = g }
          state' =
              updateGameState state { ourRandomGen = g' } move
          (g, g') = split (ourRandomGen state)




genMoveRand :: GameState -> Move
genMoveRand state =
    if length moveList'' == 0
    then Pass color
    else StoneMove (Stone (p, color))

    where
      p = pick g moveList''
      -- p = moveList'' !! r
      -- (r, _g') = randomR (0, (l'' - 1)) g
      -- l'' = length moveList''

      moveList'' =
          -- trace ("genMove, moveList'': " ++ show resMoveList'')
          resMoveList''
          where
            resMoveList'' = filter (not . isEyeLike) moveList'
      moveList' =
          -- trace ("genMove, moveList': " ++ show resMoveList')
          resMoveList'
          where
            resMoveList' = filter (not . isSuicide') moveList
      moveList =
          -- trace ("genMove, moveList: " ++ show resMoveList)
          resMoveList
          where
            resMoveList = (freeVertices bsize allStones)
                          \\ (koBlocked state)

      bsize = (boardsize state)
      allStones = (stones state)
      color = (toMove state)
      g = (ourRandomGen state)

      isSuicide' :: Vertex -> Bool
      isSuicide' v = isSuicide bsize (Stone (v, color)) allStones

      isEyeLike :: Vertex -> Bool
      isEyeLike v =
          (length vs == length sns)
          && isSuicide bsize (Stone (v, (otherColor color))) allStones
          where
            vs = adjacentVertices bsize v
            sns = filter (\(Stone (_p', c')) -> color == c') ns
            ns = neighbourStones bsize allStones (Stone (v, color))


pick :: StdGen -> [a] -> a
pick g as =
    as !! i
    where
      (i, _g) = randomR (0, ((length as) - 1)) g

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
