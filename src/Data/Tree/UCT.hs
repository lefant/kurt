{-# OPTIONS -O2 -Wall -Werror -Wwarn #-}
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
                     , backpropagate
                     , updateRaveMap
                     , updateNodeVisits
                     , updateNodeValue
                     , getLeaf
                     , UCTHeuristic
                     , UCTEvaluator
                     ) where


import Data.Maybe (fromMaybe, fromJust)
import Data.List (unfoldr, maximumBy, foldl')
import qualified Data.Map as M
import Data.Ord (comparing)
import Data.Tree (Tree(..))
import Data.Tree.Zipper (TreeLoc, tree, hasChildren, parent, getChild, findChild, modifyTree, modifyLabel)

-- import Debug.TraceOrId (trace)

import Data.Tree.UCT.GameTree



-- rootNode :: (UCTMove a) => [a] -> UCTTreeLoc a
-- rootNode moves =
--     expandNode
--     -- (fromTree $ newMoveNode (error "move at rootNode is undefined") (0.5, 1))
--     (fromTree $ newMoveNode (last moves) (0.5, 1000))
--     constantHeuristic
--     moves


-- selection section
-----------------------------------

type UCTPolicy a = (Int -> [(MoveNode a, Int)] -> Int)
-- selects a leaf according to in-tree selection policy

selectLeafPath :: (UCTMove a) => UCTPolicy a -> UCTTreeLoc a
               -> (UCTTreeLoc a, [a])
selectLeafPath policy loc =
    -- trace ("selectLeafPath " ++ show (map nodeMove $ pathToLeaf leaf))
    (leaf, map nodeMove $ pathToLeaf leaf)
    where
      leaf = selectChild policy loc

selectChild :: (UCTMove a) => UCTPolicy a -> UCTTreeLoc a
           -> UCTTreeLoc a
selectChild policy initLoc =
    selectNode initLoc
    where
      selectNode loc =
          if hasChildren loc
          then
              selectNode $ fromJust $ getChild selectedId loc
          else
              loc
          where
            selectedId = policy parentVisits numberedChildren
            numberedChildren = zip (map rootLabel $ subForest $ tree loc) [1..]
            parentVisits = nodeVisits $ rootLabel $ tree loc


principalVariation :: (UCTMove a) => UCTTreeLoc a -> [MoveNode a]
principalVariation loc =
    -- pathToLeaf $ selectLeaf policyMaxUCTValue loc
    pathToLeaf $ selectChild policyMaxRobust loc


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


-- computes list of moves needed to reach the passed leaf loc from the root
pathToLeaf :: UCTMove a => UCTTreeLoc a -> [MoveNode a]
pathToLeaf initLoc =
    -- trace ("pathToLeaf " ++ show path)
    path
    where
      path = reverse $ unfoldr f initLoc
      f loc =
        fmap (\loc' -> (rootLabel $ tree loc, loc')) $ parent loc

getLeaf :: UCTMove a => UCTTreeLoc a -> [a] -> UCTTreeLoc a
getLeaf root moves =
  foldl' chooseChild root moves
  where
    chooseChild loc move =
      case findChild ((move ==) . nodeMove . rootLabel) loc of
        Just loc' -> loc'
        Nothing -> error ("selectChild failed to find move at loc: " ++
                          show (move, loc))

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

      subForestFromMoves =
          map (\m -> newMoveNode m (h m))

constantHeuristic :: UCTMove a => UCTHeuristic a
constantHeuristic _move = (0.5, 1)



-- simulation needs to be handled exclusively by game code
-----------------------------------






-- backpropagation section
-----------------------------------



-- updates node with a new value
updateNode :: UCTMove a => Value -> MoveNode a -> MoveNode a
updateNode value node =
    -- trace ("updateNodeValue "
    --        ++ show (node, node', value)
    --       )
    node'
    where
      node' =
          node { nodeVisits = newVisits
               , nodeValue = newValue
               }
      newValue = ((oldValue * fromIntegral oldVisits) + value)
                       / fromIntegral newVisits
      oldValue = nodeValue node
      newVisits = succ oldVisits
      oldVisits = nodeVisits node

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


type UCTEvaluator a = a -> Value
type UCTUpdater a = Value -> MoveNode a -> MoveNode a

backpropagate :: UCTMove a => UCTEvaluator a -> UCTUpdater a -> UCTTreeLoc a -> UCTTreeLoc a
backpropagate evaluator updater loc =
    case parent loc' of
      Nothing ->
          -- trace "backpropagate reached root node"
          loc'
      Just parentLoc ->
          -- trace ("backpropagate "
          --        ++ show ((nodeMove $ rootLabel $ tree loc), value))
          backpropagate evaluator updater parentLoc
    where
      loc' = modifyLabel (updater value) loc
      value = evaluator (nodeMove $ rootLabel $ tree loc)


updateRaveMap :: (UCTMove a, Ord a) => RaveMap a -> UCTEvaluator a -> [a] -> RaveMap a
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
