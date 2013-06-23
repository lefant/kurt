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
                             , UCTTreeLoc(..)
                             , UCTKey
                             , newUctTree
                             , selectChild
                             , selectLeafPath
                             , selectLeafPathMoveNodes
                             , selectSubtree
                             , rootNodeVisits
                             , childNodes
                             , expandNode
                             , backpropagate
                             ) where


import           Control.DeepSeq        (NFData)
import qualified Data.HashMap.Lazy      as H
import qualified Data.IntSet            as S
import           Data.List              (foldl', unfoldr)

import           Data.Goban.ZobristHash (ZHash)
import           Data.Tree.UCT.Types



data UCTTreeLoc a = TreeLoc (UCTTree a, UCTKey)
instance UCTMove a => NFData (UCTTreeLoc a)
type UCTKey = Int
type UCTTree a = H.HashMap UCTKey (Entry a)
data Entry a = Entry { moveNode :: MoveNode a
                     , parents  :: S.IntSet
                     , children :: [UCTKey]
                     }

newUctTree :: (UCTMove a) => a -> UCTTreeLoc a
newUctTree fakeMove =
    TreeLoc (H.singleton 0 (newEntry fakeMove), 0)

newUctTree2 :: (UCTMove a) => a -> UCTKey -> UCTTreeLoc a
newUctTree2 fakeMove k =
    TreeLoc (H.singleton k (newEntry fakeMove), k)

newEntry :: (UCTMove a) => a -> Entry a
newEntry fakeMove =
    Entry { moveNode = MoveNode { nodeMove = fakeMove
                                , nodeVisits = 1
                                , nodeValue = 0.5 }
          , parents = S.empty
          , children = []
          }

rootNodeVisits :: (UCTMove a) => UCTTreeLoc a -> Count
rootNodeVisits (TreeLoc (m, k)) =
    nodeVisits $ moveNode $
               H.lookupDefault (error "invalid key in rootNodeVisits") k m

childNodes :: (UCTMove a) => UCTTreeLoc a -> [MoveNode a]
childNodes (TreeLoc (m, k)) =
    map (moveNode . ((H.!) m)) $ children $ (H.!) m k

selectSubtree :: (UCTMove a) =>
                 UCTTreeLoc a -> ZHash -> a -> UCTTreeLoc a
selectSubtree (TreeLoc (m, _)) hash fakeMove =
    case H.lookup hash m of
      Just _ -> TreeLoc(m, hash)
      Nothing -> newUctTree fakeMove

selectLeafPathMoveNodes :: (UCTMove a) =>
                           UCTPolicy a -> UCTTreeLoc a -> [MoveNode a]
selectLeafPathMoveNodes policy loc@(TreeLoc (m, _k)) =
    map keyToMove keys
    where
      keyToMove k = moveNode $ (H.!) m k
      keys = map snd $ snd $ selectLeafPath policy loc

selectChild :: (UCTMove a) => UCTPolicy a -> UCTTreeLoc a -> UCTTreeLoc a
selectChild policy loc =
    fst $ selectLeafPath policy loc

selectLeafPath :: (UCTMove a) => UCTPolicy a -> UCTTreeLoc a
               -> (UCTTreeLoc a, [(a, UCTKey)])
selectLeafPath policy loc@(TreeLoc (m, _k)) =
    (TreeLoc (m, snd $ head rpath), reverse rpath)
    where
      -- path = reverse $ map locToMove rpath
      -- locToMove (TreeLoc (m, k)) = nodeMove $ moveNode $ (H.!) m k
      rpath = unfoldr selector loc
      selector = selectNode2 policy

selectNode2 :: (UCTMove a) => UCTPolicy a -> UCTTreeLoc a ->
               Maybe ((a, UCTKey), UCTTreeLoc a)
selectNode2 policy loc@(TreeLoc (m, _k)) =
    fmap (\(move, k') -> ((move, k'), TreeLoc (m, k'))) $ selectNode policy loc

selectNode :: (UCTMove a) => UCTPolicy a -> UCTTreeLoc a -> Maybe (a, UCTKey)
selectNode policy (TreeLoc (m, k)) =
    if null childIds
    then Nothing
    else Just (selectedMove, selectedId)
    where
      childIdPairs = map idToChildIdPair childIds
      idToChildIdPair childId =
          (child, childId)
          where
            child =
                moveNode $ H.lookupDefault (error "invalid childId") childId m
      childIds = children currentEntry
      (selectedMove, selectedId) = policy parentVisits childIdPairs
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


backpropagate :: UCTMove a => (a -> Value)
              -> (Value -> MoveNode a -> MoveNode a)
              -> [UCTKey]
              -> UCTTreeLoc a
              -> UCTTreeLoc a
backpropagate evaluator updater path (TreeLoc (m, k)) =
    TreeLoc (foldl' (flip (H.adjust adjustEntry)) m path, k)
    where
      adjustEntry entry =
          entry { moveNode = updater value mnode }
          where
            value = evaluator $ nodeMove mnode
            mnode = moveNode entry
