{-# OPTIONS -Wall -Werror -Wwarn #-}
{-# LANGUAGE FlexibleInstances #-}

{- |
   Module     : Data.Tree.UCT
   Copyright  : Copyright (C) 2010 Fabian Linzberger
   License    : GNU GPL, version 3 or above

   Maintainer : Fabian Linzberger <e@lefant.net>
   Stability  : experimental
   Portability: probably

UCT tree search using Data.Tree.Zipper for updates in the Tree

-}

module Data.Tree.UCT ( selectLeafPath
                     , principalVariation
                     , policyUCB1
                     , policyRaveUCB1
                     , expandNode
                     , constantHeuristic
                     , updateRaveMap
                     , updateNodeVisits
                     , updateNodeValue
                     , UCTHeuristic
                     , backpropagate  -- re-exported
                     , getLeaf        -- re-exported
                     ) where


import           Data.List             (foldl', maximumBy)
import qualified Data.Map              as M
import           Data.Maybe            (fromMaybe)
import           Data.Ord              (comparing)

import           Data.Tree.UCT.GameMap
import           Data.Tree.UCT.Types


-- selection section
-----------------------------------

-- principalVariation :: (UCTMove a) => UCTTreeLoc a -> [a]
-- principalVariation loc@(TreeLoc (m, k)) =
--     snd $ selectLeafPath policyMaxRobust loc


policyMaxRobust :: UCTMove a => UCTPolicy a
policyMaxRobust = policyMaker (\_ n -> nodeVisits n)

-- policyMaxUCTValue :: UCTMove a => UCTPolicy a
-- policyMaxUCTValue node =
--     maximumBy
--     (comparing (nodeValue . rootLabel))
--     $ subForest node





policyUCB1 :: UCTMove a => Int -> UCTPolicy a
policyUCB1 exploratoryC =
    policyMaker (ucb1 exploratoryC)


policyRaveUCB1 :: (UCTMove a, Ord a) => Int -> Int -> RaveMap a -> UCTPolicy a
policyRaveUCB1 exploratoryC raveWeight m =
    policyMaker combinedVal
    where
      combinedVal parentVisits node =
          -- trace ("combinedVal "
          --        ++ show (nodeMove node, total, (raveVal, raveCount), (uctVal, uctCount), beta, parentVisits))
          total
          where
            total = beta * raveVal + (1 - beta) * uctVal

            beta = fromIntegral raveCount
                   / (intSum + intSum / fromIntegral raveWeight)
            intSum = fromIntegral $ raveCount + uctCount

            (raveVal, raveCount) = fromMaybe (0.5, 0) (M.lookup move m)
            uctVal = ucb1 exploratoryC parentVisits node
            uctCount = nodeVisits node
            move = nodeMove node

ucb1 :: UCTMove a => Int -> Count -> MoveNode a -> Value
ucb1 exploratoryCPercent parentVisits node =
    -- trace ("ucb1: "
    --        ++ show (nodeMove node, oldValue, ucb1part, value))
    value
    where
      value = oldValue
              + ucb1part

      oldValue = nodeValue node

      ucb1part =
          exploratoryC
             * sqrt
                   (log (fromIntegral parentVisits)
                    / (fromIntegral (nodeVisits node) + 1))

      exploratoryC = fromIntegral exploratoryCPercent / 100



policyMaker :: (Ord b) => (Int -> MoveNode a -> b) -> UCTPolicy a
policyMaker val parentVisits numberedChildren =
    snd $ maximumBy
            (comparing ((val parentVisits) . fst))
            numberedChildren



-- expansion
----------------------------------

constantHeuristic :: UCTMove a => UCTHeuristic a
constantHeuristic _move = (0.5, 1)




-- backpropagation section
-----------------------------------

updateNodeValue :: UCTMove a => Value -> MoveNode a -> MoveNode a
updateNodeValue value node =
    node'
    where
      node' = node { nodeValue = newValue }
      newValue = ((oldValue * fromIntegral oldVisits) + value)
                       / fromIntegral newVisits
      oldValue = nodeValue node
      oldVisits = pred newVisits
      newVisits = nodeVisits node

updateNodeVisits :: UCTMove a => Value -> MoveNode a -> MoveNode a
updateNodeVisits _value node = node { nodeVisits = succ $ nodeVisits node }



updateRaveMap :: (UCTMove a, Ord a) =>
                 RaveMap a -> (a -> Value) -> [a] -> RaveMap a
updateRaveMap initMap evaluator moves =
    foldl' updateOneMove initMap moves

    where
      updateOneMove m move =
          case M.lookup move m of
            Just (oldValue, oldVisits) ->
                M.insert move (newValue, newVisits) m
                where
                  newValue =
                      ((oldValue * fromIntegral oldVisits) + value)
                      / fromIntegral newVisits
                  newVisits = succ oldVisits
            Nothing ->
                M.insert move (value, 1) m
          where
            value = evaluator move
