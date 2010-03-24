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

module Data.Goban.Utils ( maxIntSet

                        , rateScore
                        , winningScore
                        , influenceFromWinrate
                        ) where


-- import Data.List ((\\))
import qualified Data.IntSet as S
import Text.Printf (printf)

import Data.Goban.Types
import Data.Tree.UCT (UCTEvaluator)
import Data.Tree.UCT.GameTree (Value)

-- import Debug.TraceOrId (trace)



-- maxString :: (Eq a) => (a -> [a]) -> (a -> Bool) -> a -> [a]
-- maxString genF filterF p =
--     maxString' [p] []
--     where
--       maxString' [] gs = gs
--       maxString' (n : ns) gs =
--           maxString' (ns ++ (((fgen n) \\ gs) \\ ns)) (n : gs)
--       fgen n =
--           filter filterF $ genF n


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



rateScore :: Score -> UCTEvaluator Move
rateScore score move =
    if score == 0
    then 0.5
    else
        if winningScore color score
        then
            -- trace ("scoreToResult winning" ++ show (color, thisScore))
            -- 0.9 + bonus
            1.0
        else
            -- trace ("scoreToResult losing" ++ show (color, thisScore))
            -- 0.1 - bonus
            0.0
    where
      -- bonus =
      --     (sqrt $ max 99 $ abs $ realToFrac score) / 100

      color = case move of
                Move (Stone _p c) -> c
                Pass c -> c
                Resign c -> error ("rateScore encountered resign " ++ show c)



winningScore :: Color -> Score -> Bool
winningScore color thisScore =
    case color of
      Black ->
          -- trace ("winningScore Black " ++ show (thisScore > 0))
          thisScore > 0
      White ->
          -- trace ("winningScore White " ++ show (thisScore < 0))
          thisScore < 0


influenceFromWinrate :: Color -> Value -> String
influenceFromWinrate color v =
    printf " %.2f" $ (if color == Black then id else negate) $ ((v - 0.5) * 2)