{-# OPTIONS -O2 -Wall -Werror -Wwarn #-}

{- |
   Module     : Data.Goban.Utils
   Copyright  : Copyright (C) 2010 Fabian Linzberger
   License    : GNU GPL, version 3 or above

   Maintainer : Fabian Linzberger <e@lefant.net>
   Stability  : experimental
   Portability: probably

Various utility functions for Goban Implementation

-}

module Data.Goban.Utils ( maxString
                        , scoreToResult
                        , winningScore
                        , verticesFromStones
                        , stoneColor
                        , otherColor
                        , inBounds
                        , nonEdgeVertices
                        , allVertices
                        ) where


import Data.List ((\\))

import Data.Goban.Goban

-- import Debug.Trace (trace)



maxString :: (Eq a) => (a -> [a]) -> (a -> Bool) -> a -> [a]
maxString genF filterF p =
    maxString' [p] []
    where
      maxString' [] gs = gs
      maxString' (n : ns) gs =
          maxString' (ns ++ (((fgen n) \\ gs) \\ ns)) (n : gs)
      fgen n =
          filter filterF $ genF n



scoreToResult :: Color -> Score -> Double
scoreToResult color thisScore =
    if thisScore == 0
    then 0.5
    else
        if winningScore color thisScore
        then
            -- trace ("scoreToResult winning" ++ show (color, thisScore))
            -- 0.9 + bonus
            1.0
        else
            -- trace ("scoreToResult losing" ++ show (color, thisScore))
            -- 0.1 - bonus
            0.0
    -- where
    --   bonus =
    --       ((sqrt . (max 99) . abs) (realToFrac thisScore)) / 100


winningScore :: Color -> Score -> Bool
winningScore color thisScore =
    case color of
      Black ->
          -- trace ("winningScore Black " ++ show (thisScore > 0))
          thisScore > 0
      White ->
          -- trace ("winningScore White " ++ show (thisScore < 0))
          thisScore < 0





----- mostly random stuff that should be reorganized below
----------------------------------------------------------


verticesFromStones :: [Stone] -> [Vertex]
verticesFromStones ss = map (\(Stone (p, _c)) -> p) ss



-- moveColor :: Move -> Color
-- moveColor (StoneMove stone) = stoneColor stone
-- moveColor (Pass color) = color

stoneColor :: Stone -> Color
stoneColor (Stone (_vertex, color)) = color


otherColor :: Color -> Color
otherColor Black = White
otherColor White = Black



inBounds :: Boardsize -> Vertex -> Bool
inBounds boardsize (x, y) =
    and [x > 0, x <= boardsize, y > 0, y <= boardsize]


nonEdgeVertices :: Boardsize -> [Vertex]
nonEdgeVertices boardsize =
    [(x, y) | x <- [lower .. upper], y <- [lower .. upper]]
    where
      upper = boardsize - lower + 1
      lower =
          if boardsize >= 9
          then 3
          else 2

allVertices :: Boardsize -> [Vertex]
allVertices n =
    [(x, y) | y <- reverse [1 .. n], x <- [1 .. n]]
