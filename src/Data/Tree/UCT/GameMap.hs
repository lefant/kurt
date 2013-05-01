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

module Data.Tree.UCT.GameMap ( UCTMap
                             , UCTMapEntry
                             , newUctTree
                             ) where


import qualified Data.HashMap.Lazy   as H
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
selectChild policy (TreeLoc (m, k)) =
    TreeLoc (m, selectNode k)
    where
      selectNode k =
          if null childIds
          then k
          else selectedId
          where
            childIdPairs = map idToChildIdPair childIds
            idToChildIdPair id =
                (child, id)
                where
                  child =
                      moveNode $ H.lookupDefault (error "invalid childId") id m
            childIds = children currentEntry
            selectedId = policy parentVisits childIdPairs
            parentVisits = nodeVisits $ moveNode currentEntry
            currentEntry = (H.!) m k
