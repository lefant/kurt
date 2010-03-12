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
   Module     : Data.Goban.DataMap
   Copyright  : Copyright (C) 2010 Fabian Linzberger
   License    : GNU GPL, version 3 or above

   Maintainer : Fabian Linzberger <e@lefant.net>
   Stability  : experimental
   Portability: probably

Goban Implementation using Data.Map

-}

module Data.Goban.DataMap (
                           DataMapGoban(..)
                          ) where

import Data.Map as M (Map, empty, insert, delete, lookup, keys)
import Data.List ((\\), foldl')
import Data.Goban.Utils

newtype DataMapGoban = DataMapGoban (Int, Map (Int, Int) Color)
    deriving (Show)

instance Goban DataMapGoban where

    addStone (DataMapGoban (boardsize, goban)) (Stone (vertex, color)) =
        DataMapGoban (boardsize, goban')
        where
          goban' = M.insert vertex color goban

    deleteStones (DataMapGoban (boardsize, goban)) stones =
        DataMapGoban (boardsize, goban')
        where
          goban' = foldl' f goban stones
          f tmpGoban (Stone (p, _)) =
              M.delete p tmpGoban

    freeVertices (DataMapGoban (boardsize, goban)) =
        (allVertices boardsize) \\ (M.keys goban)

    vertexToStone (DataMapGoban (_, goban)) p =
        case M.lookup p goban of
          Just color -> Just $ Stone (p, color)
          Nothing -> Nothing

    sizeOfGoban (DataMapGoban (boardsize, _)) = boardsize

    newGoban boardsize =
        DataMapGoban (boardsize, M.empty)

