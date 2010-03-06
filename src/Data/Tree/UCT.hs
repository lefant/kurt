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
                     , selectNode
                     , policyUCB1
                     , expandNode
                     , backpropagate
                     ) where


import Data.Maybe (fromJust)
import Data.List (unfoldr, maximumBy)
import Data.Ord (comparing)
import Data.Word (Word)
import Data.Tree (Tree(..))
import Data.Tree.Zipper (TreeLoc, tree, fromTree, isRoot, hasChildren, parent, findChild, modifyTree, modifyLabel)
-- import Debug.Trace (trace)

import Data.Tree.UCT.GameTree


exploratoryC :: Double
exploratoryC = 0.4



rootNode :: (UCTNode a) => [a] -> UCTTreeLoc a
rootNode moves =
    fromTree $ Node (nodeFromMove undefined) $ subForestFromMoves moves


-- selection section
-----------------------------------

type UCTPolicy a = (UCTTree a -> UCTTree a)
-- selects a leaf according to in-tree selection policy
-- default UCB1, should be pluggable
selectNode :: (UCTNode a) => UCTPolicy a -> UCTTreeLoc a
           -> (UCTTreeLoc a, [a])
selectNode policy loc =
    (leaf, leafPath leaf)
    where
      leaf = head $ snd $ span hasChildren $ iterate selectNode' loc
      selectNode' loc' =
          -- fromMaybe (error ("selectNode' findChild returned Nothing "
          --                   ++ show (selectedTree, loc)))
          fromJust $ findChild ((==) selectedTree) loc'
          where
            selectedTree = policy $ tree loc'


policyUCB1 :: UCTNode a => UCTPolicy a
policyUCB1 node =
    maximumBy
    (comparing ((ucb1 parentVisits) . rootLabel))
    $ subForest node
    where
      parentVisits = nodeVisits $ rootLabel node

ucb1 :: UCTNode a => Word -> MoveNode a -> Double
ucb1 parentVisits node =
    (nodeValue node)
    + (exploratoryC
       * (sqrt
          ((log (fromIntegral parentVisits))
           / (fromIntegral (nodeVisits node)))))



-- expansion
----------------------------------

expandNode :: UCTNode a => UCTTreeLoc a -> [a] -> UCTTreeLoc a
expandNode loc children =
    modifyTree expandChildren loc
    where
    expandChildren node =
        node { subForest = subForestFromMoves children }

subForestFromMoves :: UCTNode a => [a] -> UCTForest a
subForestFromMoves moves =
    map (((flip Node) []) . nodeFromMove) moves


-- simulation needs to be handled by game code
-----------------------------------






-- backpropagation section
-----------------------------------

-- computes list of moves needed to reach the passed leaf loc from the root
leafPath :: UCTNode a => UCTTreeLoc a -> [a]
leafPath initLoc =
    reverse $ unfoldr f initLoc
    where
      f loc =
          case parent loc of
            Just loc' ->
                Just (nodeMove $ rootLabel $ tree loc, loc')
            Nothing ->
                Nothing


-- updates node with a new value
updateNodeValue :: UCTNode a => Double -> MoveNode a -> MoveNode a
updateNodeValue value node =
    node { nodeVisits = newVisits
         , nodeValue = ((oldValue * (fromIntegral oldVisits)) + value)
                       / fromIntegral newVisits
         }
    where
      oldVisits = nodeVisits node
      newVisits = succ oldVisits
      oldValue = nodeValue node


backpropagate :: UCTNode a => Double -> UCTTreeLoc a -> UCTTreeLoc a
backpropagate value loc =
    if isRoot loc
    then loc'
    else backpropagate value' loc'
    where
      loc' = modifyLabel (updateNodeValue value') loc
      value' = updateBackpropagationValue (nodeMove $ rootLabel $ tree $ loc) value

