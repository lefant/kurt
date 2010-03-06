{-# OPTIONS -O2 -Wall -Werror -Wwarn -XFlexibleInstances #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

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
                              , MoveNode(..)
                              , nodeFromMove
                              , UCTNode(..)
                              -- , UctNode(..)
                              -- , UctLabel(..)
                              -- , defaultUctLabel
                              ) where


import Data.Tree (Tree, Forest)
import Data.Tree.Zipper (TreeLoc)

import Data.Word (Word)

-- import Control.Monad.Random (Rand, RandomGen)
-- import Text.Printf (printf)

-- import Debug.Trace (trace)



type UCTTreeLoc a = TreeLoc (MoveNode a)
type UCTTree a = Tree (MoveNode a)
type UCTForest a = Forest (MoveNode a)



-- things the game logic must be able to provide for moves
class (Eq a, Show a) => UCTNode a where
    initialMoveValue :: a -> Double
    updateBackpropagationValue :: a -> Double -> Double


data MoveNode a = MoveNode { nodeMove   :: a
                           , nodeVisits :: !Word
                           , nodeValue  :: Double
                           }
                  deriving (Show)

instance (UCTNode a) => Eq (MoveNode a) where
    (==) a b = nodeMove a == nodeMove b

-- instance (Show a) => Show (MoveNode a) where
--     show node = show $ nodeMove node

nodeFromMove :: (UCTNode a) => a -> MoveNode a
nodeFromMove move = MoveNode { nodeMove = move
                             , nodeVisits = 0
                             , nodeValue = initialMoveValue move }
















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

