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
                             , newMoveNode
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

newMoveNode :: (UCTMove a) => a -> (Value, Count) -> [UCTKey]-> UCTMapEntry a
newMoveNode move (value, visits) parents0 =
    -- FIXME: incomplete
    Entry { moveNode = MoveNode { nodeMove = move
                                , nodeVisits = visits
                                , nodeValue = value }
          , parents = parents0
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
