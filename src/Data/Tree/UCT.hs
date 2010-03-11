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

module Data.Tree.UCT ( rootNode
                     , selectLeafPath
                     , principalVariation
                     , policyUCB1
                     , policyRaveUCB1
                     , expandNode
                     , backpropagate
                     , updateRaveMap
                     ) where


import Data.Maybe (fromJust)
import Data.List (unfoldr, maximumBy, foldl')
import qualified Data.Map as M
import Data.Ord (comparing)
import Data.Word (Word)
import Data.Tree (Tree(..))
import Data.Tree.Zipper (TreeLoc, tree, fromTree, hasChildren, parent, findChild, modifyTree, modifyLabel)

-- import Debug.Trace (trace)

import Data.Tree.UCT.GameTree


exploratoryC :: Double
exploratoryC = 1

raveWeight :: Double
raveWeight = 3000


rootNode :: (UCTNode a) => [a] -> UCTTreeLoc a
rootNode moves =
    expandNode
    (fromTree $
     Node
     (nodeFromMove
       -- (last moves)
       (error "move at rootNode is undefined")
     )
     []
    )
    moves


-- selection section
-----------------------------------

type UCTPolicy a = (UCTTree a -> UCTTree a)
-- selects a leaf according to in-tree selection policy
-- default UCB1, should be pluggable

selectLeafPath :: (UCTNode a) => UCTPolicy a -> UCTTreeLoc a
               -> (UCTTreeLoc a, [a])
selectLeafPath policy loc =
    -- trace ("selectLeafPath " ++ show (map nodeMove $ pathToLeaf leaf))
    (leaf, map nodeMove $ pathToLeaf leaf)
    where
      leaf = selectLeaf policy loc

selectLeaf :: (UCTNode a) => UCTPolicy a -> UCTTreeLoc a
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

policyUCB1 :: UCTNode a => UCTPolicy a
policyUCB1 node =
    maximumBy
    (comparing ((ucb1 parentVisits) . rootLabel))
    $ subForest node
    where
      parentVisits = nodeVisits $ rootLabel node

ucb1 :: UCTNode a => Word -> MoveNode a -> Value
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

policyMaxRobust :: UCTNode a => UCTPolicy a
policyMaxRobust node =
    maximumBy
    (comparing (nodeVisits . rootLabel))
    $ subForest node

principalVariation :: (UCTNode a) => UCTTreeLoc a -> [MoveNode a]
principalVariation loc =
    pathToLeaf $ selectLeaf policyMaxRobust loc



policyRaveUCB1 :: (UCTNode a, Ord a) => RaveMap a -> UCTPolicy a
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
                                     Nothing -> (0, 0)
            uctVal = ucb1 parentVisits node
            uctCount = nodeVisits node
            move = nodeMove node

      parentVisits = nodeVisits $ rootLabel parentNode


-- computes list of moves needed to reach the passed leaf loc from the root
pathToLeaf :: UCTNode a => UCTTreeLoc a -> [(MoveNode a)]
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

expandNode :: UCTNode a => UCTTreeLoc a -> [a] -> UCTTreeLoc a
expandNode loc children =
    -- trace ("expandNode " ++ show children)
    modifyTree expandChildren loc
    where
    expandChildren node =
        -- trace ("expandChildren " ++ show (subForest n))
        n
        where
          n = node { subForest = subForestFromMoves children }

subForestFromMoves :: UCTNode a => [a] -> UCTForest a
subForestFromMoves moves =
    map (((flip Node) []) . nodeFromMove) moves


-- simulation needs to be handled exclusively by game code
-----------------------------------






-- backpropagation section
-----------------------------------



-- updates node with a new value
updateNodeValue :: UCTNode a => Value -> MoveNode a -> MoveNode a
updateNodeValue value node =
    -- trace ("updateNodeValue "
    --        ++ show (nodeMove node, value)
    --       )
    node { nodeVisits = newVisits
         , nodeValue = ((oldValue * (fromIntegral oldVisits)) + value)
                       / fromIntegral newVisits
         }
    where
      oldVisits = nodeVisits node
      newVisits = succ oldVisits
      oldValue = nodeValue node


backpropagate :: UCTNode a => Value -> UCTTreeLoc a -> UCTTreeLoc a
backpropagate value loc =
    case parent loc' of
      Nothing ->
          -- trace "backpropagate reached root node"
          loc'
      Just parentLoc ->
          -- trace ("backpropagate "
          --        ++ show ((nodeMove $ rootLabel $ tree loc), value))
          backpropagate value' parentLoc
    where
      loc' = modifyLabel (updateNodeValue value) loc
      value' =
          updateBackpropagationValue (nodeMove $ rootLabel $ tree $ loc) value


updateRaveMap :: (UCTNode a, Ord a) => RaveMap a -> Value -> [a] -> RaveMap a
updateRaveMap initMap v moves =
    foldl' updateOneMove initMap moves

    where
      updateOneMove :: (UCTNode a, Ord a) => RaveMap a -> a -> RaveMap a
      updateOneMove (RaveMap m) move =
          case M.lookup move m of
            Just (oldValue, oldVisits) ->
                RaveMap $ M.insert move (newValue, newVisits) m
                where
                  newValue =
                      ((oldValue * (fromIntegral oldVisits)) + v)
                      / fromIntegral newVisits
                  newVisits = succ oldVisits
            Nothing ->
                RaveMap $ M.insert move (v, 1) m
