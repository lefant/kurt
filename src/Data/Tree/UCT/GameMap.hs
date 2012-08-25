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


type UCTKey = Int
type UCTMap a = H.HashMap UCTKey (UCTMapEntry a)
data UCTMapEntry a = Entry { moveNode :: MoveNode a
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
