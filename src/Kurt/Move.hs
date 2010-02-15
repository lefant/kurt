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


-- import System.Random (split, randomR, StdGen)
import System.Random (RandomGen)
-- import Control.Monad.Random (Rand, getRandomR)
-- import Data.List ((\\))
-- import Data.Map as M (Map, fromList, toList, insertWith, assocs)
import Debug.Trace (trace)


import Data.Goban.Utils
import Data.Goban (GameState(..), saneMoves)
import Data.Tree.UCT





genMove :: (RandomGen g) => GameState -> Color -> g -> Move
genMove state color rGen =
    if null (saneMoves state color)
    then Pass color
    else
        if (winningProb bestMove) < 0.1
           then Resign color
           else
               case moveHistory $ nodeState bestMove of
                 [] -> error "genMove: moveHistory of bestMove is empty"
                 moves -> last moves
    where
      bestMove =
          trace ("genMoves: " ++ show pv)
          head pv
      pv = uct state (simulCount state) rGen

