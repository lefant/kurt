{-# OPTIONS -Wall -Werror -Wwarn #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE FlexibleInstances #-}

{- |
   Module     : Data.Tree.UCT
   Copyright  : Copyright (C) 2010,2012 Fabian Linzberger
   License    : GNU GPL, version 3 or above

   Maintainer : Fabian Linzberger <e@lefant.net>
   Stability  : experimental
   Portability: probably

UCT tree search using Data.Tree.Zipper for updates in the Tree

-}

module Data.Tree.UCT.GameTree ( UCTTreeLoc
                              , UCTTree
                              , UCTForest
                              , UCTMove
                              , newMoveNode
                              ) where


import           Data.Tree           (Forest, Tree (..))
import           Data.Tree.Zipper    (TreeLoc)

import           Data.Tree.UCT.Types (Count, MoveNode (..), UCTMove, Value)


-- import Debug.TraceOrId (trace)



type UCTTreeLoc a = TreeLoc (MoveNode a)
type UCTTree a = Tree (MoveNode a)
type UCTForest a = Forest (MoveNode a)

newMoveNode :: (UCTMove a) => a -> (Value, Count) -> UCTTree a
newMoveNode move (value, visits) =
    -- trace ("newMoveNode: " ++ show (move, value, visits))
    Node { rootLabel = MoveNode { nodeMove = move
                                , nodeVisits = visits
                                , nodeValue = value }
         , subForest = [] }
