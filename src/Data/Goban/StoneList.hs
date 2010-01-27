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
   Module     : Data.Goban.StoneList
   Copyright  : Copyright (C) 2010 Fabian Linzberger
   License    : GNU GPL, version 3 or above

   Maintainer : Fabian Linzberger <e@lefant.net>
   Stability  : experimental
   Portability: probably

Goban Implementation using a list of (Vertex, Color) tuples. Looking
up status of a vertex using toStone is slow, because it needs to scan
the whole list.

-}

module Data.Goban.StoneList (
                             StoneListGoban(..)
                            ,freeVertices
                            ,isSuicide
                            ,neighbourStones
                            ) where

import Data.List ((\\))
import Data.Goban.Utils

newtype StoneListGoban = StoneListGoban (Int, [Stone])
    deriving (Show)

instance Goban StoneListGoban where

    addStone (StoneListGoban (boardsize, stones)) s =
        StoneListGoban (boardsize, (s : stones))

    deleteStones (StoneListGoban (boardsize, stones)) ss =
        StoneListGoban (boardsize, stones \\ ss)

    freeVertices (StoneListGoban (boardsize, stones)) =
        (allVertices boardsize) \\ (verticesFromStones stones)

    vertexToStone (StoneListGoban (_b, stones)) p =
        fmap (\color -> Stone (p, color)) result
        where
          result = lookup p $ map (\(Stone s) -> s) stones

    clearGoban goban =
        StoneListGoban (boardsize, [])
        where
          boardsize = sizeFromGoban goban

    sizeFromGoban (StoneListGoban (boardsize, _)) = boardsize

    newGoban boardsize = StoneListGoban (boardsize, [])
