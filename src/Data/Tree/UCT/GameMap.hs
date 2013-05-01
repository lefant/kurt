{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE FlexibleInstances #-}

{- |
   Module     : Data.Tree.UCT.GameMap
   Copyright  : Copyright (C) 2010 Fabian Linzberger
   License    : GNU GPL, version 3 or above

   Maintainer : Fabian Linzberger <e@lefant.net>
   Stability  : experimental
   Portability: probably

UCT tree search using a zobrist hash keyed HashMap of MoveNodes

-}

module Data.Tree.UCT.GameMap ( UCTTree
                             , UCTTreeLoc
                             , newUctTree
                             , selectChild
                             , selectLeafPath
                             ) where


import qualified Data.HashMap.Lazy   as H
import           Data.List           (unfoldr)
import           Data.Tree.UCT.Types


data UCTTreeLoc a = TreeLoc (UCTTree a, UCTKey)
type UCTKey = Int
type UCTTree a = H.HashMap UCTKey (Entry a)
data Entry a = Entry { moveNode :: MoveNode a
                     , parents  :: [UCTKey]
                     , children :: [UCTKey]
                     }

newUctTree :: (UCTMove a) => a -> UCTTreeLoc a
newUctTree fakeMove =
    TreeLoc (H.singleton 0 entry, 0)
    where
      entry =
          Entry { moveNode = MoveNode { nodeMove = fakeMove
                                      , nodeVisits = 1
                                      , nodeValue = 0.5 }
                , parents = []
                , children = []
          }


selectChild :: (UCTMove a) => UCTPolicy a -> UCTTreeLoc a -> UCTTreeLoc a
selectChild policy loc =
    fst $ selectLeafPath policy loc

selectLeafPath :: (UCTMove a) => UCTPolicy a -> UCTTreeLoc a
               -> (UCTTreeLoc a, [a])
selectLeafPath policy loc =
    (leaf, path)
    where
      path = reverse $ map locToMove rpath
      locToMove (TreeLoc (m, k)) = nodeMove $ moveNode $ (H.!) m k
      leaf = head rpath
      rpath = unfoldr selector loc
      selector = selectNode2 policy

selectNode2 :: (UCTMove a) => UCTPolicy a -> UCTTreeLoc a ->
               Maybe (UCTTreeLoc a, UCTTreeLoc a)
selectNode2 policy loc0 =
    fmap (\loc -> (loc, loc)) $ selectNode policy loc0

selectNode :: (UCTMove a) => UCTPolicy a -> UCTTreeLoc a -> Maybe (UCTTreeLoc a)
selectNode policy (TreeLoc (m, k)) =
    if null childIds
    then Nothing
    else Just (TreeLoc (m, selectedId))
    where
      childIdPairs = map idToChildIdPair childIds
      idToChildIdPair childId =
          (child, childId)
          where
            child =
                moveNode $ H.lookupDefault (error "invalid childId") childId m
      childIds = children currentEntry
      selectedId = policy parentVisits childIdPairs
      parentVisits = nodeVisits $ moveNode currentEntry
      currentEntry = (H.!) m k
