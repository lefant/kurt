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

module Data.Tree.UCT.GameTree ( UctNode(..)
                              , UctLabel(..)
                              , defaultUctLabel
                              ) where



import Control.Monad.Random (Rand, RandomGen)
import Text.Printf (printf)
-- import Debug.Trace (trace)



class (Show a) => UctNode a where
    isTerminalNode :: a -> Bool
    finalResult :: a -> Float
    randomEvalOnce :: (RandomGen g) => a -> Rand g Float
    children :: a -> [a]

instance (UctNode a) => Show (UctLabel a) where
    show label =
        show (nodeState label) ++ " " ++
        printf "%.2f " (winningProb label) ++
        show (visits label) ++ dStr ++ " - "
        where
          dStr = case isDone label of
                   True -> "+ "
                   False -> ""

data UctLabel a = UctLabel {
     nodeState       :: a
    ,winningProb     :: Float
    ,visits          :: Int
    ,isDone          :: Bool
    }



instance Eq (UctLabel a) where
    (==) a b =
        (winningProb a == winningProb b)
        && (visits a == visits b)
        && (isDone a == isDone b)


defaultUctLabel :: UctLabel a
defaultUctLabel = UctLabel {
                    nodeState = undefined
                  , winningProb = 0.5
                  -- , uctValue = 0.5
                  , visits = 1
                  , isDone = False
                  }

