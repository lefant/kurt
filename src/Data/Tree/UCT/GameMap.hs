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
                             , expandNode
                             ) where


import qualified Data.HashMap.Lazy      as H
import qualified Data.IntSet            as S
import           Data.List              (foldl', unfoldr)

import           Data.Goban.ZobristHash (ZHash)
import           Data.Tree.UCT.Types



data UCTTreeLoc a = TreeLoc (UCTTree a, UCTKey)
type UCTKey = Int
type UCTTree a = H.HashMap UCTKey (Entry a)
data Entry a = Entry { moveNode :: MoveNode a
                     , parents  :: S.IntSet
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
                , parents = S.empty
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



expandNode :: UCTMove a => UCTTreeLoc a -> UCTHeuristic a -> [(a, ZHash)]
           -> UCTTreeLoc a
expandNode (TreeLoc (m, k)) heu moveHashPairs =
    TreeLoc (m'', k)
    where
      m'' = H.insert k currentEntry' m'
      currentEntry' = currentEntry { children = map snd moveHashPairs }
      currentEntry = (H.!) m' k
      m' = foldl' ensureEntry m moveHashPairs
      ensureEntry m0 (move, hash) =
          case H.lookup hash m0 of
            Nothing ->
                H.insert hash (childEntry move) m0
            Just entry ->
                H.insert hash (addParent entry) m0
      childEntry move = Entry { moveNode = mNode move
                              , parents = S.singleton k
                              , children = [] }
          where
            mNode nmove = MoveNode { nodeMove = nmove
                                   , nodeVisits = visits
                                   , nodeValue = value
                                   }
                where
                  (value, visits) = heu move
      addParent entry =
          entry { parents = S.insert k $ parents entry }
