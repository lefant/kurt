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
import Data.List

import Data.Goban
-- import Debug.Trace (trace)




genMove :: GameState -> Move
genMove state =
    if l'' == 0
    then Pass color
    else StoneMove (Stone (p, color))

    where
      p = moveList''' !! r
      (r, _g') = randomR (0, (l'' - 1)) g

      l'' = length moveList''

      -- moveList''' = drop ((length moveList'') `div` 2) moveList''
      moveList''' =
          -- trace ("genMove, moveList'': " ++ show resMoveList'')
          resMoveList'''
          where
            resMoveList''' = map snd $ sort $ map whatifScore moveList''

            whatifScore v =
                (modifier * (runOneRandom (updateGameState state move)), v)
                where
                  move = StoneMove (Stone (v, color))


            modifier = case color of
                         Black -> -1
                         White -> 1

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
    if l'' == 0
    then Pass color
    else StoneMove (Stone (p, color))

    where
      p = moveList'' !! r
      (r, _g') = randomR (0, (l'' - 1)) g

      l'' = length moveList''

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
