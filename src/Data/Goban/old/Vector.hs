{-# OPTIONS -O2 -Wall -Werror -Wwarn #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

{- |
   Module     : Data.Goban.Vector
   Copyright  : Copyright (C) 2010 Fabian Linzberger
   License    : GNU GPL, version 3 or above

   Maintainer : Fabian Linzberger <e@lefant.net>
   Stability  : experimental
   Portability: probably

Goban Implementation using Data.Vector

-}

module Data.Goban.Vector (
                         VectorGoban(..)
                        ) where

import qualified Data.Vector.Unboxed as V
-- import Debug.Trace (trace)


import Data.Goban.Goban


newtype VectorGoban = VectorGoban (Int, (V.Vector Int))
    deriving (Show)

data VertexState = VertexColor Color | Empty
                 deriving (Show, Eq)

instance Goban VectorGoban where

    addStone (VectorGoban (boardsize, goban)) (Stone (vertex, color)) =
        -- trace ("addStone: " ++ show (intVertex, intState))
        VectorGoban (boardsize, goban')
        where
          goban' = goban V.// [(intVertex, intState)]
          intVertex = vertexToInt boardsize vertex
          intState = stateToInt (VertexColor color)


    deleteStones (VectorGoban (boardsize, goban)) stones =
        VectorGoban (boardsize, goban')
        where
          goban' = goban V.// (map intPair stones)
          intPair (Stone (vertex, _)) =
              (intVertex, intState)
              where
                intVertex = vertexToInt boardsize vertex
                intState = stateToInt Empty

    freeVertices (VectorGoban (boardsize, goban)) =
        map (intToVertex boardsize) $ V.toList $
            V.findIndices ((stateToInt Empty) ==) goban

    vertexToStone (VectorGoban (boardsize, goban)) p =
        -- trace ("vertexToStone " ++ show (p, intVertex))
        result
        where
          result =
              case intToState $ goban V.! intVertex of
                VertexColor color -> Just $ Stone (p, color)
                Empty -> Nothing
          intVertex = vertexToInt boardsize p

    sizeOfGoban (VectorGoban (boardsize, _)) = boardsize

    newGoban boardsize =
        VectorGoban
        (boardsize,
         V.replicate
              (1 + (vertexToInt boardsize (boardsize, boardsize)))
              (stateToInt Empty))



-- helpers: vertex / integer conversion

vertexToInt :: Int -> Vertex -> Int
vertexToInt boardsize (x, y)
    | x > boardsize = error "vertexToInt: x > boardsize"
    | x < 1 = error "vertexToInt: x < 1"
    | y > boardsize = error "vertexToInt: y > boardsize"
    | y < 1 = error "vertexToInt: y < 1"
vertexToInt boardsize (x, y) =
    y' * boardsize + x'
    where
      x' = x - 1
      y' = y - 1

{-# INLINE vertexToInt #-}

intToVertex :: Int -> Int -> Vertex
intToVertex boardsize n
    | n < 0 = error "intToVertex: n < 0"
    | n > (boardsize ^ (2 :: Int)) = error "intToVertex: n > (boardsize ^ 2)"
intToVertex boardsize n =
    (x, y)
    where
      y = (n `div` boardsize) + 1
      x = (n `mod` boardsize) + 1


{-# INLINE intToVertex #-}


stateToInt :: VertexState -> Int
stateToInt Empty = 0
stateToInt (VertexColor Black) = 1
stateToInt (VertexColor White) = 2

intToState :: Int -> VertexState
intToState 0 = Empty
intToState 1 = VertexColor Black
intToState 2 = VertexColor White
intToState _ = error "intToState parameter out of range"