{-# OPTIONS -O2 -Wall -Werror -Wwarn -XFlexibleInstances #-}

{-
Copyright (C) 2010 Fabian Linzberger <e@lefant.net>

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.
-}

{- |
   Module     : Data.Tree.UCT
   Copyright  : Copyright (C) 2010 Fabian Linzberger
   License    : GNU GPL, version 3 or above

   Maintainer : Fabian Linzberger <e@lefant.net>
   Stability  : experimental
   Portability: probably


UCT tree search using Data.Tree.Zipper for updates in the Tree

-}

module Data.Tree.UCT (
                      uct
                     ,UctNode(..)
                     ,UctLabel(..)
                     ) where


import Control.Monad.Random (Rand, RandomGen, getRandomR, evalRand)
import Data.Maybe (fromJust)
import Data.List (unfoldr, sort)
import Data.Tree (Tree(..))
import Data.Tree.Zipper (TreeLoc, tree, fromTree, hasChildren, setTree, parent, getChild)
import Debug.Trace (trace)



class (Show a) => UctNode a where
    isTerminalNode :: a -> Bool
    finalResult :: a -> Float
    randomEvalOnce :: (RandomGen g) => a -> Rand g Float
    children :: a -> [a]

instance (UctNode a) => Show (UctLabel a) where
    show label =
        show (nodeState label) ++ " " ++
        (take 4 $ show $ winningProb label) ++ " " ++
        show (runs label) ++ dStr ++ " - "
        where
          dStr = case isDone label of
                   True -> "+ "
                   False -> ""
                 

data UctLabel a = UctLabel {
     nodeState       :: a
    ,winningProb     :: Float
    ,runs            :: Int
    ,isDone          :: Bool
    }



instance Eq (UctLabel a) where
    (==) a b =
        (winningProb a == winningProb b)
        && (runs a == runs b)
        && (isDone a == isDone b)

instance Ord (Tree (UctLabel b)) where
    compare x y =
        case compare (f x) (f y) of
          EQ -> compare (g x) (g y)
          order -> order
        where
          f = winningProb . rootLabel
          g = runs . rootLabel

defaultUctLabel :: UctLabel a
defaultUctLabel = UctLabel {
                    nodeState = undefined
                  , winningProb = 0.5
                  , runs = 0
                  , isDone = False
                  }


uct :: (UctNode a, RandomGen g) => a -> Int -> g -> [(UctLabel a)]
uct initState n rGen =
    principalVariation $ tree $ evalRand (uctZipper (fromTree $ makeNodeWithChildren initState) n) rGen


uctZipper :: (UctNode a, RandomGen g) =>
             TreeLoc (UctLabel a) ->
             Int ->
             Rand g (TreeLoc (UctLabel a))
uctZipper loc 0 = return loc
uctZipper loc n = do
  (loc', done) <- uctZipperDown loc
  case done of
    True -> return loc'
    False -> uctZipper loc' (n-1)


uctZipperDown  :: (UctNode a, RandomGen g) => TreeLoc (UctLabel a) -> Rand g ((TreeLoc (UctLabel a)), Bool)
uctZipperDown loc =
    if hasChildren loc
    then
        (do
          childLoc <- chooseOne loc
          uctZipperDown childLoc)
    else
        if isTerminalNode state
        then uctZipperUp loc (finalResult state) True
        else (do
               result <- randomEvalOnce state
               uctZipperUp loc' result False)
    where
      state = nodeState $ rootLabel node
      node = tree loc

      loc' = setTree node' loc
      node' = node { subForest = makeSubForest state }



uctZipperUp  :: (UctNode a, RandomGen g) => TreeLoc (UctLabel a) -> Float -> Bool -> Rand g ((TreeLoc (UctLabel a)), Bool)
uctZipperUp loc result done =
    case parent loc' of
      Nothing ->
          return (loc', done)
      Just parentLoc ->
          -- if evaluating this subtree is finished
          if done
          then
              -- one perfect move is enough to make parent
              -- a losing move
              if result == 1
              then
                  -- trace ("uctZipperUp: result 1, done "
                  --            ++ show ("parent", rootLabel parentNode)
                  --            ++ show ("label'", label')
                  --           )
                  uctZipperUp parentLoc 0 True
              else
                  -- if all siblings are also done, then parent
                  -- is also done with 1 - (max of sibling scores)
                  if all (isDone . rootLabel) $ subForest parentNode
                  then
                      -- trace ("uctZipperUp: all done "
                      --        ++ show ("parent", rootLabel parentNode)
                      --        ++ show ("label'", label')
                      --        ++ show ("result''", result'')
                      --        ++ "\n\nsubforest\n"
                      --        ++ (show $ map rootLabel $ subForest parentNode)
                      --       )
                      uctZipperUp parentLoc result'' True
                  else
                      -- trace ("uctZipperUp: done, but active siblings left " ++ show label' ++ (show $ map rootLabel $ filter (not . isDone . rootLabel) $ subForest parentNode))
                      uctZipperUp parentLoc result' False
          else
              uctZipperUp parentLoc result' False
          where
            parentNode = tree parentLoc
            maxResult = winningProb $ rootLabel $ maximum $ subForest parentNode
            result'' = 1 - maxResult
    where
      loc' = setTree node' loc
      node' = node { rootLabel = label' }
      label' = label { winningProb = newProb, runs = (oldCount + 1), isDone = done }

      newProb =
          if done
          then result
          else updateProb oldProb oldCount result
      oldProb = winningProb label
      oldCount = runs label
      label = rootLabel node
      node = tree loc

      result' = 1 - result



makeNodeWithChildren :: (UctNode a) => a -> Tree (UctLabel a)
makeNodeWithChildren state =
    Node { rootLabel = defaultUctLabel { nodeState = state }
         , subForest = makeSubForest state
         }

makeSubForest :: (UctNode a) => a -> [Tree (UctLabel a)]
makeSubForest state =
    map makeLeafNode $ children state

makeLeafNode :: (UctNode a) => a -> Tree (UctLabel a)
makeLeafNode state =
    Node { rootLabel = defaultUctLabel { nodeState = state },
           subForest = [] }




chooseOne :: (UctNode a, RandomGen g) => TreeLoc (UctLabel a) -> Rand g (TreeLoc (UctLabel a))
chooseOne loc = do
  index <- weightedChoose weightedList
  return $ fromJust $ getChild index loc
      where
        weightedList =
            -- trace ("activeSubtrees "
            --        ++ (show $ map (rootLabel . fst) numberedForest)
            --        ++ " after filtering "
            --        ++ (show $ map (rootLabel.fst) activeSubtrees))
            map weightTree activeSubtrees
        activeSubtrees =
            filter (not . isDone . rootLabel . fst) numberedForest
        numberedForest = zip (subForest (tree loc)) [1..]


weightTree :: (Tree (UctLabel a), Int) -> (Float, Int)
weightTree (node, n) =
    (prob, n)
    where
      prob = winningProb $ rootLabel node

weightedChoose :: (RandomGen g) => [(Float, a)] -> Rand g a
weightedChoose [] = error "weightedChoose called with empty list"
weightedChoose as = do
    i <- getRandomR (0, tot)
    return $ pickF i as'
    where
      tot = sum (map fst as')
      as' = map fstToInt as


fstToInt :: (Float, a) -> (Int, a)
fstToInt (a, b) =
    ((truncate (a * 1000) :: Int), b)

pickF :: (Ord a, Num a) => a -> [(a, t)] -> t
pickF n ((k,x):xs)
    | n <= k    = x
    | otherwise = pickF (n-k) xs
pickF _ _  = error "pick used with empty list"



updateProb :: Float -> Int -> Float -> Float
updateProb oldProb oldCount result =
    ((oldProb * (fromIntegral oldCount)) + result) / (fromIntegral (oldCount + 1))



principalVariation :: (UctNode a) => Tree (UctLabel a) -> [(UctLabel a)]
principalVariation t =
    trace ("PV alternate first moves: " ++ (concatMap (show.rootLabel) $ take 5 $ reverse $ sort $ subForest t) ++ "\n")
    unfoldr maxChild t

maxChild :: Tree (UctLabel a) -> Maybe ((UctLabel a), Tree (UctLabel a))
maxChild t =
    case subForest t of
      [] -> Nothing
      forest ->
          Just (rootLabel mNode, mNode)
              where
                mNode = maximum forest
