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
                 ) where

import Data.List

import Data.Goban
import Debug.Trace (trace)


genMove :: GameState -> Color -> Move
genMove state color =
    case moveList''' of
      [] -> Pass color
      (p : _) -> StoneMove (Stone (p, color))

    where
      moveList''' = drop ((length moveList'') `div` 2) moveList''
      moveList'' =
          trace ("genMove, moveList'': " ++ show resMoveList'')
          resMoveList''
          where
            resMoveList'' = filter (not . isEyeLike) moveList'
      moveList' =
          trace ("genMove, moveList': " ++ show resMoveList')
          resMoveList'
          where
            resMoveList' = filter (not . isSuicide') moveList
      moveList =
          trace ("genMove, moveList: " ++ show resMoveList)
          resMoveList
          where
            resMoveList = (freeVertices bsize allStones)
                          \\ (koBlocked state)

      bsize = (boardsize state)
      allStones = (stones state)

      isSuicide' :: Vertex -> Bool
      isSuicide' p = isSuicide bsize (Stone (p, color)) allStones

      isEyeLike :: Vertex -> Bool
      isEyeLike p =
          (length (neighbourStonesSameColor bsize (Stone (p, color)) allStones)) == (length (adjacentVertices bsize p))
