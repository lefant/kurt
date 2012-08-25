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


type UCTMap a = H.HashMap Int (UCTMapEntry a)
type UCTMapEntry a = (MoveNode a)


newMoveNode :: (UCTMove a) => a -> (Value, Count) -> UCTMapEntry a
newMoveNode move (value, visits) =
    -- FIXME: incomplete
    MoveNode { nodeMove = move
                                , nodeVisits = visits
                                , nodeValue = value }
