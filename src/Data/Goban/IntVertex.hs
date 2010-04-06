{-# OPTIONS -O2 -Wall -Werror -Wwarn #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

{- |
   Module     : Data.Goban.IntVertex
   Copyright  : Copyright (C) 2010 Fabian Linzberger
   License    : GNU GPL, version 3 or above

   Maintainer : Fabian Linzberger <e@lefant.net>
   Stability  : experimental
   Portability: probably


helpers: vertex / integer conversion

-}

module Data.Goban.IntVertex ( intToVertex
                            , vertexToInt
                            , intBorderVertices
                            , maxIntIndex
                            , intAscAdjacentVertices
                            ) where


import Data.Goban.Types (Boardsize, Vertex, borderVertices)

-- import Debug.TraceOrId (trace)



type IntVertex = Int


intBorderVertices :: Boardsize -> [IntVertex]
intBorderVertices n =
    map (vertexToInt n) $ borderVertices n

{-# INLINE vertexToInt #-}
vertexToInt :: Boardsize -> Vertex -> IntVertex
vertexToInt n (x, y)
    | x > n + 1 = error "vertexToInt: x > n + 1"
    | x < 0 = error "vertexToInt: x < 0"
    | y > n + 1 = error "vertexToInt: y > n + 1"
    | y < 0 = error "vertexToInt: y < 0"
vertexToInt n (x, y) =
    y * (n + 2) + x

{-# INLINE intToVertex #-}
intToVertex :: Boardsize -> IntVertex -> Vertex
intToVertex n i
    | i < 0 = error "intToVertex: n < 0"
    | i > (maxIntIndex n + 1) =
        error "intToVertex: i > size n"
intToVertex n i =
    (x, y)
    where
      y = i `div` (n + 2)
      x = i `mod` (n + 2)


{-# INLINE maxIntIndex #-}
maxIntIndex :: Boardsize -> IntVertex
maxIntIndex n = vertexToInt n (n + 1, n + 1)

-- {-# INLINE maxEdge #-}
-- maxEdge :: Boardsize -> Int
-- maxEdge n = n + 1


intAscAdjacentVertices :: Boardsize -> IntVertex -> [IntVertex]
intAscAdjacentVertices n vertex =
    -- make sure to be in ascending order
    map (vertexToInt n) [(x,y-1),(x-1,y),(x+1,y),(x,y+1)]
    where
      (x, y) = (intToVertex n) vertex
