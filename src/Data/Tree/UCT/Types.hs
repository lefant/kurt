{-# OPTIONS -Wall -Werror -Wwarn #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE FlexibleInstances #-}

{- |
   Module     : Data.Tree.UCT.Types
   Copyright  : Copyright (C) 2012 Fabian Linzberger
   License    : GNU GPL, version 3 or above

   Maintainer : Fabian Linzberger <e@lefant.net>
   Stability  : experimental
   Portability: probably

UCT tree search, shared types

-}

module Data.Tree.UCT.Types ( UCTMove
                           , MoveNode(..)
                           , UCTHeuristic
                           , UCTPolicy
                           , RaveValue
                           , RaveMap
                           , newRaveMap
                           , Count
                           , Value
                           ) where

import           Control.DeepSeq (NFData)
import qualified Data.Map        as M
import           Text.Printf     (printf)


type Value = Float
-- type Value = Double
type Count = Int


class (Eq a, Show a) => UCTMove a


data MoveNode a = MoveNode { nodeMove   :: !a
                           , nodeValue  :: !Value
                           , nodeVisits :: !Count
                           }

instance UCTMove a => NFData (MoveNode a)

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


type UCTPolicy a =
    (Int -> [(MoveNode a, Int)] -> (a, Int))
type UCTHeuristic a = a -> (Value, Count)


type RaveValue = (Value, Count)

type RaveMap a = M.Map a RaveValue

newRaveMap :: (UCTMove a) => RaveMap a
newRaveMap = M.empty
