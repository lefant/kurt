{-# OPTIONS -O2 -Wall -Werror -Wwarn #-}

{- |
   Module     : Data.Goban.Utils
   Copyright  : Copyright (C) 2010 Fabian Linzberger
   License    : GNU GPL, version 3 or above

   Maintainer : Fabian Linzberger <e@lefant.net>
   Stability  : experimental
   Portability: probably

Various utility functions for Goban Implementation (without direct dependencies on either GameState or Goban.hs

-}

module Data.Goban.Utils ( maxString
                        , maxIntSet

                        , scoreToResult
                        , winningScore

                        , centerHeuristic
                        ) where


import Data.List ((\\))
import qualified Data.IntSet as S

import Data.Goban.Types
import Data.Tree.UCT.GameTree (Value, Count)

-- import Debug.TraceOrId (trace)



maxString :: (Eq a) => (a -> [a]) -> (a -> Bool) -> a -> [a]
maxString genF filterF p =
    maxString' [p] []
    where
      maxString' [] gs = gs
      maxString' (n : ns) gs =
          maxString' (ns ++ (((fgen n) \\ gs) \\ ns)) (n : gs)
      fgen n =
          filter filterF $ genF n


maxIntSet :: (Int -> S.IntSet) -> (Int -> Bool) -> Int -> S.IntSet
maxIntSet genF filterF p =
    maxIntSet' (S.singleton p) S.empty
    where
      maxIntSet' is js
          | S.null is = js
          | otherwise = maxIntSet' is'' js'
          where
            is'' = S.union is' $ S.difference ks js
            js' = S.insert i js
            ks = S.filter filterF $ genF i
            (i, is') = S.deleteFindMin is



scoreToResult :: Color -> Score -> Value
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



centerHeuristic :: Boardsize -> Move -> (Value, Count)
centerHeuristic n (Move (Stone (x, y) _color)) =
    -- trace ("centerHeuristic " ++ show (x, y, result))
    result
    where
      result = 
          (0.2
           + (fromIntegral
              (minimum [ x - 1, n - x, y - 1, n - y, 3]) / 5),
           1000)
centerHeuristic _ _ = error "centerHeuristic received non StoneMove arg"


