{-# OPTIONS -O2 -Wall -Werror -Wwarn -XFlexibleInstances #-}

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
                     , backpropagate
                     , updateRaveMap
                     , UCTHeuristic
                     , UCTEvaluator
                     ) where


import Data.Maybe (fromJust)
import Data.List (unfoldr, maximumBy, foldl')
import qualified Data.Map as M
import Data.Ord (comparing)
import Data.Tree (Tree(..))
import Data.Tree.Zipper (TreeLoc, tree, hasChildren, parent, findChild, modifyTree, modifyLabel)

-- import Debug.TraceOrId (trace)

import Data.Tree.UCT.GameTree


exploratoryC :: Value
exploratoryC = 0.4

raveWeight :: Value
raveWeight = 1.5


-- rootNode :: (UCTMove a) => [a] -> UCTTreeLoc a
-- rootNode moves =
--     expandNode
--     -- (fromTree $ newMoveNode (error "move at rootNode is undefined") (0.5, 1))
--     (fromTree $ newMoveNode (last moves) (0.5, 1000))
--     constantHeuristic
--     moves


-- selection section
-----------------------------------

type UCTPolicy a = (UCTTree a -> UCTTree a)
-- selects a leaf according to in-tree selection policy
-- default UCB1, should be pluggable

selectLeafPath :: (UCTMove a) => UCTPolicy a -> UCTTreeLoc a
               -> (UCTTreeLoc a, [a])
selectLeafPath policy loc =
    -- trace ("selectLeafPath " ++ show (map nodeMove $ pathToLeaf leaf))
    (leaf, map nodeMove $ pathToLeaf leaf)
    where
      leaf = selectLeaf policy loc

selectLeaf :: (UCTMove a) => UCTPolicy a -> UCTTreeLoc a
           -> UCTTreeLoc a
selectLeaf policy initLoc =
    selectNode initLoc
    where
      selectNode loc =
          if hasChildren loc
          then
              selectNode $ fromJust $ findChild ((==) selectedTree) loc
          else
              loc
          where
            selectedTree = policy $ tree loc

policyUCB1 :: UCTMove a => UCTPolicy a
policyUCB1 node =
    maximumBy
    (comparing ((ucb1 parentVisits) . rootLabel))
    $ subForest node
    where
      parentVisits = nodeVisits $ rootLabel node

ucb1 :: UCTMove a => Count -> MoveNode a -> Value
ucb1 parentVisits node =
    -- trace ("ucb1: "
    --        ++ show (nodeMove node, oldValue, ucb1part, value))
    value
    where
      value = oldValue
              + ucb1part

      oldValue = nodeValue node

      ucb1part =
          exploratoryC
             * (sqrt
                ((log (fromIntegral parentVisits))
                 / ((fromIntegral (nodeVisits node) + 1))))

policyMaxRobust :: UCTMove a => UCTPolicy a
policyMaxRobust node =
    maximumBy
    (comparing (nodeVisits . rootLabel))
    $ subForest node

principalVariation :: (UCTMove a) => UCTTreeLoc a -> [MoveNode a]
principalVariation loc =
    pathToLeaf $ selectLeaf policyMaxRobust loc


policyRaveUCB1 :: (UCTMove a, Ord a) => RaveMap a -> UCTPolicy a
policyRaveUCB1 (RaveMap m) parentNode =
    maximumBy
    (comparing (combinedVal . rootLabel))
    $ subForest parentNode
    where
      combinedVal node =
          beta * raveVal + (1 - beta) * uctVal
          where
            beta = fromIntegral raveCount
                   / (intSum + intSum / raveWeight)
            intSum = fromIntegral $ raveCount + uctCount

            (raveVal, raveCount) = case M.lookup move m of
                                     Just p -> p
                                     Nothing -> (0.5, 0)
            uctVal = ucb1 parentVisits node
            uctCount = nodeVisits node
            move = nodeMove node

      parentVisits = nodeVisits $ rootLabel parentNode




-- computes list of moves needed to reach the passed leaf loc from the root
pathToLeaf :: UCTMove a => UCTTreeLoc a -> [(MoveNode a)]
pathToLeaf initLoc =
    -- trace ("pathToLeaf " ++ show path)
    path
    where
      path = reverse $ unfoldr f initLoc
      f loc =
          case parent loc of
            Just loc' ->
                Just (rootLabel $ tree loc, loc')
            Nothing ->
                Nothing


-- expansion
----------------------------------

type UCTHeuristic a = a -> (Value, Count)


expandNode :: UCTMove a => UCTTreeLoc a -> UCTHeuristic a -> [a] -> UCTTreeLoc a
expandNode loc h children =
    -- trace ("expandNode " ++ show children)
    modifyTree expandChildren loc
    where
      expandChildren node =
          n
          where
            n = node { subForest = subForestFromMoves children }

      subForestFromMoves moves =
          map (\m -> newMoveNode m (h m)) moves

constantHeuristic :: UCTMove a => UCTHeuristic a
constantHeuristic _move = (0.5, 1)



-- simulation needs to be handled exclusively by game code
-----------------------------------






-- backpropagation section
-----------------------------------



-- updates node with a new value
updateNodeValue :: UCTMove a => Value -> MoveNode a -> MoveNode a
updateNodeValue value node =
    -- trace ("updateNodeValue "
    --        ++ show (node, node', value)
    --       )
    node'
    where
      node' =
          node { nodeVisits = newVisits
               , nodeValue = newValue
               }
      newValue = ((oldValue * (fromIntegral oldVisits)) + value)
                       / fromIntegral newVisits
      oldValue = nodeValue node
      newVisits = succ oldVisits
      oldVisits = nodeVisits node


type UCTEvaluator a = a -> Value

backpropagate :: UCTMove a => UCTEvaluator a -> UCTTreeLoc a -> UCTTreeLoc a
backpropagate evaluator loc =
    case parent loc' of
      Nothing ->
          -- trace "backpropagate reached root node"
          loc'
      Just parentLoc ->
          -- trace ("backpropagate "
          --        ++ show ((nodeMove $ rootLabel $ tree loc), value))
          backpropagate evaluator parentLoc
    where
      loc' = modifyLabel (updateNodeValue value) loc
      value = evaluator (nodeMove $ rootLabel $ tree $ loc)


updateRaveMap :: (UCTMove a, Ord a) => RaveMap a -> UCTEvaluator a -> [a] -> RaveMap a
updateRaveMap initMap evaluator moves =
    foldl' updateOneMove initMap moves

    where
      updateOneMove (RaveMap m) move =
          case M.lookup move m of
            Just (oldValue, oldVisits) ->
                RaveMap $ M.insert move (newValue, newVisits) m
                where
                  newValue =
                      ((oldValue * (fromIntegral oldVisits)) + value)
                      / fromIntegral newVisits
                  newVisits = succ oldVisits
            Nothing ->
                RaveMap $ M.insert move (value, 1) m
          where
            value = evaluator move
