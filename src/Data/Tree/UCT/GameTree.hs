{-# OPTIONS -O2 -Wall -Werror -Wwarn -XFlexibleInstances #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE BangPatterns #-}

{- |
   Module     : Data.Tree.UCT
   Copyright  : Copyright (C) 2010 Fabian Linzberger
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
                              , MoveNode(..)
                              , newMoveNode
                              , RaveValue
                              , RaveMap(..)
                              , newRaveMap
                              , Count
                              , Value
                              -- , UctNode(..)
                              -- , UctLabel(..)
                              -- , defaultUctLabel
                              ) where


import Data.Tree (Tree(..), Forest)
import Data.Tree.Zipper (TreeLoc)
import qualified Data.Map as M
import Text.Printf (printf)

-- import Debug.TraceOrId (trace)



type UCTTreeLoc a = TreeLoc (MoveNode a)
type UCTTree a = Tree (MoveNode a)
type UCTForest a = Forest (MoveNode a)

type Value = Double
type Count = Int


class (Eq a, Show a) => UCTMove a


data MoveNode a = MoveNode { nodeMove   :: a
                           , nodeValue  :: !Value
                           , nodeVisits :: !Count
                           }

instance (UCTMove a) => Show (MoveNode a) where
    show node =
        "(" ++ show (nodeVisits node) ++ dStr
        ++ printf "/%.2f) " (nodeValue node)
        ++ show (nodeMove node)
        where
          dStr = ""
          -- dStr = case isDone label of
          --          True -> "+ "
          --          False -> ""


instance (UCTMove a) => Eq (MoveNode a) where
    (==) a b = nodeMove a == nodeMove b

-- instance (Show a) => Show (MoveNode a) where
--     show node = show $ nodeMove node

newMoveNode :: (UCTMove a) => a -> (Value, Count) -> UCTTree a
newMoveNode move (value, visits) =
    -- trace ("newMoveNode: " ++ show (move, value, visits))
    Node { rootLabel = MoveNode { nodeMove = move
                                , nodeVisits = visits
                                , nodeValue = value }
         , subForest = [] }




type RaveValue = (Value, Count)

newtype RaveMap a = RaveMap (M.Map a RaveValue)

newRaveMap :: (UCTMove a) => RaveMap a
newRaveMap = RaveMap M.empty










-- class (Show a) => UctNode a where
--     isTerminalNode :: a -> Bool
--     finalResult :: a -> Float
--     randomEvalOnce :: (RandomGen g) => a -> Rand g Float
--     children :: a -> [a]

-- instance (UctNode a) => Show (UctLabel a) where
--     show label =
--         show (nodeState label) ++ " " ++
--         printf "%.2f " (winningProb label) ++
--         show (visits label) ++ dStr ++ " - "
--         where
--           dStr = case isDone label of
--                    True -> "+ "
--                    False -> ""

-- data UctLabel a = UctLabel {
--      nodeState       :: a
--     ,winningProb     :: Float
--     ,visits          :: Int
--     ,isDone          :: Bool
--     }

-- instance Eq (UctLabel a) where
--     (==) a b =
--         (winningProb a == winningProb b)
--         && (visits a == visits b)
--         && (isDone a == isDone b)


-- defaultUctLabel :: UctLabel a
-- defaultUctLabel = UctLabel {
--                     nodeState = undefined
--                   , winningProb = 0.5
--                   -- , uctValue = 0.5
--                   , visits = 1
--                   , isDone = False
--                   }

