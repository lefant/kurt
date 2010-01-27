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
   Module     : Data.Goban.Array
   Copyright  : Copyright (C) 2010 Fabian Linzberger
   License    : GNU GPL, version 3 or above

   Maintainer : Fabian Linzberger <e@lefant.net>
   Stability  : experimental
   Portability: probably

Goban Implementation using Data.Array

-}

module Data.Goban.Array (
                         ArrayGoban(..)
                        ) where

import Data.Array.Diff
import Data.Goban.Utils

newtype ArrayGoban = ArrayGoban (Int, (DiffArray (Int, Int) VertexState))
    deriving (Show)

data VertexState = VertexColor Color | Empty
                 deriving (Show, Eq)

instance Goban ArrayGoban where

    addStone (ArrayGoban (bsize, goban)) (Stone (vertex, color)) =
        ArrayGoban (bsize, (goban // [(vertex, VertexColor color)]))

    deleteStones (ArrayGoban (bsize, goban)) stones =
        ArrayGoban (bsize, (goban // updates))
        where
          updates = zip (verticesFromStones stones) $ repeat Empty

    freeVertices (ArrayGoban (_, goban)) =
        map fst $ filter ((Empty ==) . snd) $ assocs goban

    vertexToStone (ArrayGoban (_, goban)) p =
        case goban ! p of
          VertexColor color -> Just $ Stone (p, color)
          Empty -> Nothing

    sizeOfGoban (ArrayGoban (boardsize, _)) = boardsize

    newGoban boardsize =
        ArrayGoban (boardsize,
                    (listArray ((1,1),(boardsize, boardsize))
                                   $ repeat Empty))
