{-# OPTIONS -O2 -Wall -Werror -Wwarn -XFlexibleInstances #-}

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
                     ,uctZipperDown
                     ,makeNodeWithChildren
                     ,UctNode(..)
                     ,UctLabel(..)
                     ,principalVariation
                     ) where


import Control.Monad.Random (Rand, RandomGen, evalRand)
import Data.Maybe (fromJust)
import Data.List (unfoldr, maximumBy)
import Data.Ord (comparing)
import Data.Tree (Tree(..))
import Data.Tree.Zipper (TreeLoc, tree, fromTree, hasChildren, setTree, parent, getChild)
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

exploratoryC :: Float
exploratoryC = 1

uct :: (UctNode a, RandomGen g) => a -> g -> Tree (UctLabel a)
uct initState rGen =
    tree $ evalRand (uctZipper (fromTree $ makeNodeWithChildren initState)) rGen


uctZipper :: (UctNode a, RandomGen g) =>
             TreeLoc (UctLabel a) ->
             Rand g (TreeLoc (UctLabel a))
uctZipper loc = do
  (loc', done) <- uctZipperDown loc
  case done of
    True -> return loc'
    False -> uctZipper loc'


uctZipperDown  :: (UctNode a, RandomGen g) => TreeLoc (UctLabel a) -> Rand g ((TreeLoc (UctLabel a)), Bool)
uctZipperDown loc =
    if hasChildren loc
    then
        uctZipperDown $ chooseUctMax loc
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
              if result > 0.8
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
            maxResult =
                winningProb $ rootLabel $
                            maximumBy
                            (comparing (winningProb . rootLabel))
                            $ subForest parentNode
            result'' = 1 - maxResult
    where
      loc' = setTree node' loc
      node' = node { rootLabel = label' }
      label' = label { winningProb = newProb, visits = (oldCount + 1), isDone = done }

      newProb =
          if done
          then result
          else updateProb oldProb oldCount result
      oldProb = winningProb label
      oldCount = visits label
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



chooseUctMax :: (UctNode a) => TreeLoc (UctLabel a) -> TreeLoc (UctLabel a)
chooseUctMax loc =
  -- trace ("chooseUctMax: "
  --        ++ show ((rootLabel $ tree loc),
  --                 fst uctMaxChildPair,
  --                 (uctValue parentVisits $ fst uctMaxChildPair)))
  uctMaxChild
  where
    uctMaxChild =
        fromJust $ getChild (snd uctMaxChildPair) loc

    uctMaxChildPair =
        maximumBy
        (comparing ((uctValue parentVisits) . fst))
        activeSubtrees

    parentVisits = visits $ rootLabel $ tree loc

    activeSubtrees =
        filter (not . isDone . fst) numberedForest
    numberedForest = zip (map rootLabel $ subForest (tree loc)) [1..]

uctValue :: (UctNode a) => Int -> (UctLabel a) -> Float
uctValue parentVisits node =
    -- trace ("uctValue: "
    --        ++ show node
    --        ++ show (parentVisits, winningProb node, value))
    value
    where
      value =
          (winningProb node)
          + (exploratoryC
             * (sqrt
                ((log (fromIntegral parentVisits))
                 / (fromIntegral (visits node)))))





updateProb :: Float -> Int -> Float -> Float
updateProb oldProb oldCount result =
    ((oldProb * (fromIntegral oldCount)) + result) / (fromIntegral (oldCount + 1))



principalVariation :: (UctNode a) => Tree (UctLabel a) -> [(UctLabel a)]
principalVariation t =
    unfoldr maxChild t

maxChild :: Tree (UctLabel a) -> Maybe ((UctLabel a), Tree (UctLabel a))
maxChild t =
    case subForest t of
      [] -> Nothing
      forest ->
          Just (rootLabel mNode, mNode)
              where
                mNode = maximumBy
                        (comparing (visits . rootLabel))
                        forest

