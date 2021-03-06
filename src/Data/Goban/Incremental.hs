{-# OPTIONS -Wall -Werror -Wwarn #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

{- |
   Module     : Data.Goban.Incremental
   Copyright  : Copyright (C) 2010 Fabian Linzberger
   License    : GNU GPL, version 3 or above

   Maintainer : Fabian Linzberger <e@lefant.net>
   Stability  : experimental
   Portability: probably

Incrementally updated stone chains including liberty count for goban.

-}

module Data.Goban.Incremental ( Chain(..)
                              , ChainMap
                              , GobanMap

                              , isSuicide
                              , stonesAndLiberties
                              , allStones
                              , isPotentialFullEye
                              , colorTerritories

                              , newGobanMap
                              , newChainMap
                              , showGobanMap
                              , showChainMap
                              , vertexChain
                              , addStone
                              ) where



import           Data.List           (foldl', nub, partition, sort, unfoldr)
import           Data.Maybe          (fromJust, fromMaybe, mapMaybe)
import           Text.Printf         (printf)


import qualified Data.IntMap         as M
import qualified Data.IntSet         as IS
import qualified Data.Set            as S

import qualified Data.HashMap.Strict as H

-- import Debug.TraceOrId (trace)


import           Data.Goban.Types


data Chain = Chain { chainColor     :: !Color
                   , chainLiberties :: !VertexSet
                   , chainVertices  :: !VertexSet
                   , chainNeighIds  :: !ChainNeighIds
                   , chainNeighVs   :: !VertexSet
                   }

instance Show Chain where
    show (Chain c ls vs nids nvs) =
        show c ++ " vs:" ++ unwords (map gtpShowVertex (S.elems vs))
               ++ " ls:" ++ unwords (map gtpShowVertex (S.elems ls))
               ++ "\nnids: " ++ show nids
               ++ "\nnvs: " ++ show nvs
               ++ "\n"

type ChainId = Int
noChainId :: ChainId
noChainId = 0

borderChainId :: ChainId
borderChainId = 999999


-- maybe also reconsider Vector for this?
type ChainMap = M.IntMap Chain

showChainMap :: ChainMap -> String
showChainMap cm =
    concatMap (\(k, v) -> show k ++ " " ++ show v ++ "\n") $ M.toList cm


type GobanMap = H.HashMap Vertex ChainId

type ChainNeighIds = IS.IntSet






isPotentialFullEye :: GobanMap -> Stone -> Bool
isPotentialFullEye g (Stone p color) =
  all isSameColorOrBorder as &&
          ((opponentDiagonalCount == 0) ||
           ((opponentDiagonalCount == 1) && (borderDiagonalCount == 0)))
  where
    opponentDiagonalCount = length $ filter isOtherColor ds
    borderDiagonalCount = length $ filter isBorder ds
    ds = map (vertexState g) $ diagonalVertices p
    as = map (vertexState g) $ adjacentVertices p
    isSameColorOrBorder c = c `elem` [Border, Colored color]
    isBorder c = c == Border
    isOtherColor c = c == Colored (otherColor color)




colorTerritories :: GobanMap -> [Vertex] -> [(Color, [Vertex])]
colorTerritories g t =
  case allAdjacentStonesSameColor g t of
    Just tColor -> [(tColor, t)]
    Nothing -> []

allAdjacentStonesSameColor :: GobanMap -> [Vertex] -> Maybe Color
allAdjacentStonesSameColor g ps =
  maybeSameColor $ map stoneColor $ concat as
  where
    as = map (adjacentStones g) ps
    maybeSameColor [] = Nothing
    maybeSameColor (c : cs) =
      if all (c ==) cs
      then Just c
      else Nothing

adjacentStones :: GobanMap -> Vertex -> [Stone]
adjacentStones g p = verticesToStones g $ adjacentVertices p

verticesToStones :: GobanMap -> [Vertex] -> [Stone]
verticesToStones g = mapMaybe (vertexStone g)





stonesAndLiberties :: GobanMap -> ChainMap -> Stone
                   -> (Int, Int, Int, Int, Int, Int)
stonesAndLiberties cg cm s@(Stone p color) =
    (captureC, chainC, ourMinL, otherMinL, ourTotalL, otherTotalL)

    where
      -- ourSc = stoneCount allOurChains
      -- otherSc = stoneCount allOtherChains
      captureC = length deadIds
      chainC = length allOurChains
      ourMinL = minLiberties allOurChains
      otherMinL = minLiberties allOtherChains
      ourTotalL = totalLiberties allOurChains
      otherTotalL = totalLiberties allOtherChains


      (allOurChains, allOtherChains) =
          partition ((color ==) . chainColor) $ M.elems cm4

      minLiberties cs = minimum (4 : map (S.size . chainLiberties) cs)

      totalLiberties cs =
          fromIntegral (sum $ map (S.size . chainLiberties) cs)


      cm4 = foldl' mapDeleteChain cm3 deadIds

      (cm3, deadIds) = removeNeighbourLiberties cm2 p $ IS.fromList neighIds

      cm2 = case ourIds of
              [] -> cm1
              js ->
                  mergeChains cm1 (sort (j : js)) p
              where
                (cm1, j) = mapAddChain cm s adjFrees neighIds


      adjFrees = S.fromList $ map snd adjFreePs
      -- list of adjacent same color chain ids
      ourIds = nub $ map fst ourIdPs
      -- ChainNeighbour type neighbour id - vertex map
      neighIds = map fst neighIdPs

      -- partition friend and foe
      (ourIdPs, neighIdPs) = partition ((color ==) . chainColor . getChain cm . fst) adjIdPs

      -- partition out free vertices
      (adjFreePs, adjIdPs) = partition ((== noChainId) . fst) adjPs

      -- lookup all adjacent chain ids
      adjPs = filter ((/= borderChainId) . fst) $ map readPairWithKey $ adjacentVertices p

      readPairWithKey ap = (vertexId cg ap, ap)


allStones :: ChainMap -> [Vertex]
allStones cm =
    S.toList $ S.unions $ map chainVertices $ M.elems cm





newGobanMap :: Boardsize -> GobanMap
newGobanMap n =
  H.fromList $ zip (borderVertices n) $ repeat borderChainId


showGobanMap :: GobanMap -> String
showGobanMap goban =
    unlines ([xLegend] ++
             zipWith (++) ys (zipWith (++) (map (unwords . map show) ls) ys) ++
             [xLegend])
    where
      ls = unfoldr nLines $ map idState chainIds
      nLines xs = if null xs then Nothing else Just $ splitAt n xs
      ys = map (printf " %2d ") $ reverse [1 .. n]
      xLegend = "    " ++ unwords (map ((: []) . xToLetter) [1 .. n])
      n = n1 - 1
      chainIds = filter (/= borderChainId) $ map lookupVertex $ allVertices n1
      lookupVertex = vertexId goban
      (n1, _) = maximum $ H.keys goban


isSuicide :: GobanMap -> ChainMap -> Stone -> Bool
isSuicide cg cm (Stone p color) =
    null adjFrees &&
         all (S.null . S.delete p . chainLiberties . getChain cm) ourIds' &&
             all (not . S.null . S.delete p . chainLiberties . getChain cm) neighIds'
    where
    adjPs = filter (/= borderChainId) $ map (vertexId cg) $ adjacentVertices p

    -- partition out free vertices
    (adjFrees, adjIds) = partition (== noChainId) adjPs
    -- partition friend and foe
    (ourIds, neighIds) = partition ((color ==) . chainColor . getChain cm) adjIds
    -- list of adjacent same color chain ids
    ourIds' = nub ourIds
    -- ChainNeighbour type neighbour id - vertex map
    neighIds' = nub neighIds




addStone :: GobanMap -> ChainMap -> Stone -> (GobanMap, ChainMap, [Stone])
addStone cg cm s@(Stone p color) =
  (cg4, cm5, dead)
  where
    -- lookup all adjacent chain ids
    adjPs = filter ((/= borderChainId) . fst) $ map readPairWithKey $ adjacentVertices p

    -- add new chain with played stone and
    -- merge chains becoming connected if necessary
    (cm1, j) = mapAddChain cm s adjFrees neighIds
    (cg2, cm3, i) = case ourIds of
                 [] -> (cg, cm1, j)
                 js -> (cg1, cm2, i0)
                   where
                     is@(i0 : _) = sort (j : js)
                     cm2 = mergeChains cm1 is p
                     cg1 = foldl' (\g p' -> H.insert p' i0 g) cg $
                           S.elems $ chainVertices $
                           getChain cm2 i0

    -- update neighbour chains, removing their liberties lost by current move
    -- return list of ids that as a consequence are now dead
    (cm4, deadIds) = removeNeighbourLiberties cm3 p $ IS.fromList neighIds

    dead = deadStones deadIds

    -- delete neighbour chains that just died
    (cg3, cm5) = foldl' deleteChain (cg2, cm4) deadIds

    -- write new chain id to played vertex
    cg4 = H.insert p i cg3

    -- trace ("after addStone\n" ++ showChainMap cm5) $ return ()




    -- partition out free vertices
    (adjFreePs, adjIdPs) = partition ((== noChainId) . fst) adjPs
    -- partition friend and foe
    (ourIdPs, neighIdPs) = partition ((color ==) . chainColor . getChain cm . fst) adjIdPs

    -- VertexSet of adjacent liberties
    adjFrees = S.fromList $ map snd adjFreePs
    -- list of adjacent same color chain ids
    ourIds = nub $ map fst ourIdPs
    -- ChainNeighbour type neighbour id - vertex map
    neighIds = map fst neighIdPs

    readPairWithKey ap = (vertexId cg ap, ap)

    deadStones :: [ChainId] -> [Stone]
    deadStones =
      concatMap (chainStones . getChain cm)

    chainStones :: Chain -> [Stone]
    chainStones c =
      map (flip Stone (chainColor c)) $ S.toList $ chainVertices c





deleteChain :: (GobanMap, ChainMap) -> ChainId -> (GobanMap, ChainMap)
deleteChain (cg, cm) i =
  (cg', cm')
  where
    cm' = mapDeleteChain cm i
    cg' = foldl' (flip H.delete) cg vs
    vs = S.elems $ chainVertices $ getChain cm i





vertexChain :: GobanMap -> ChainMap -> Vertex -> Chain
vertexChain cg cm p = getChain cm $ vertexId cg p

vertexStone :: GobanMap -> Vertex -> Maybe Stone
vertexStone cg p =
  case vertexState cg p of
    Colored color -> Just $ Stone p color
    Empty -> Nothing
    EmptyKoBlocked -> Nothing
    Border -> Nothing

vertexState :: GobanMap -> Vertex -> VertexState
vertexState cg v = idState $ vertexId cg v

vertexId :: GobanMap -> Vertex -> Int
vertexId cg p = H.lookupDefault noChainId p cg
{-# INLINE vertexId #-}





newChainMap :: ChainMap
newChainMap = M.empty

getChain :: ChainMap -> ChainId -> Chain
getChain cm i = fromJust $ M.lookup i cm

nextChainId :: ChainMap -> Color -> ChainId
nextChainId cm color =
    case color of
      Black ->
          if M.null cm
          then 1
          else max 1 (fst $ M.findMax cm) + 1
      White ->
          if M.null cm
          then -1
          else min (-1) (fst $ M.findMin cm) - 1

idState :: Int -> VertexState
idState i
    | i == noChainId = Empty
    | i == borderChainId = Border
    | i > 0 = Colored Black
    | i < 0 = Colored White
    | otherwise = error $ "idState unmatched " ++ show i



removeNeighbourLiberties :: ChainMap -> Vertex -> ChainNeighIds
                         -> (ChainMap, [ChainId])
removeNeighbourLiberties initCm p neighIds =
    (cm', dead)
    where
      dead = IS.toList $ IS.filter isDeadChain neighIds

      isDeadChain :: ChainId -> Bool
      isDeadChain i = S.null $ chainLiberties $ getChain cm' i

      cm' = IS.foldl' updateCM initCm neighIds

      updateCM :: ChainMap -> ChainId -> ChainMap
      updateCM cm i = M.adjust updateNC i cm

      updateNC :: Chain -> Chain
      updateNC c =
          c { chainLiberties = S.delete p $ chainLiberties c }



mapAddChain :: ChainMap -> Stone -> VertexSet -> [ChainId]
            -> (ChainMap, ChainId)
mapAddChain cm (Stone p color) adjFrees neighIds =
    (cm'' , i)
    where
      cm'' = mapAddChainNeighs cm' i neighIds (S.singleton p)
      cm' = M.insert i c cm
      c = Chain { chainColor = color
                , chainLiberties = adjFrees
                , chainVertices = S.singleton p
                , chainNeighIds = IS.fromList neighIds
                , chainNeighVs = neighVs
                }
      i = nextChainId cm color
      neighVs = S.unions $ map (chainVertices . getChain cm) neighIds

mapAddChainNeighs :: ChainMap -> ChainId -> [ChainId] -> VertexSet
                      -> ChainMap
mapAddChainNeighs initCm i neighIds neighVs =
    foldl' updateCM initCm neighIds
    where
      updateCM :: ChainMap -> ChainId -> ChainMap
      updateCM cm ni = M.adjust updateNC ni cm

      updateNC :: Chain -> Chain
      updateNC c =
          c { chainNeighIds = IS.insert i $ chainNeighIds c
            , chainNeighVs = S.union neighVs $ chainNeighVs c
            }


mergeChains :: ChainMap -> [ChainId] -> Vertex -> ChainMap
mergeChains cm ois@(i : is) p =
    -- trace ("mergeChains cm''': " ++ show cm''')
    cm3
    where
      cm3 = mergeChainsNeighbours cm2 ois neighIds
      cm2 = M.insert i c' cm1
      cm1 = foldl' (flip M.delete) cm is
      c' = c { chainLiberties = S.delete p $ S.unions $ map chainLiberties cs
             , chainVertices = S.unions $ map chainVertices cs
             , chainNeighIds = neighIds
             , chainNeighVs = neighVs
             }
      neighIds = IS.unions $ map chainNeighIds cs
      neighVs = S.unions $ map chainNeighVs cs
      cs = c : map (getChain cm) is
      c = getChain cm i
mergeChains _ [] _ = error "mergeChains called with []"

mergeChainsNeighbours :: ChainMap -> [ChainId] -> ChainNeighIds
                        -> ChainMap
mergeChainsNeighbours initCm (i : is) neighs =
    foldChains updateNC initCm $ IS.toList neighs
    where
      updateNC :: Chain -> Chain
      updateNC c =
          c { chainNeighIds = nNeighs2 }
          where
            nNeighs2 = IS.insert i nNeighs1
            nNeighs1 = nNeighs `IS.difference` IS.fromList is
            nNeighs = chainNeighIds c
mergeChainsNeighbours _ [] _ = error "mergeChainsNeighbours called with []"


mapDeleteChain :: ChainMap -> ChainId -> ChainMap
mapDeleteChain initCm i =
    M.delete i $
     foldChains updateNC initCm $
                IS.toList $ chainNeighIds $ getChain initCm i
    where
      updateNC :: Chain -> Chain
      updateNC c =
          c { chainNeighIds = IS.delete i $ chainNeighIds c
            , chainLiberties = chainLiberties c `S.union` removedNeighVs
            , chainNeighVs = chainNeighVs c `S.difference` removedStones
            }
          where
            removedNeighVs = removedStones `S.intersection` chainNeighVs c

      removedStones = chainVertices $
                      fromMaybe (error ("mapDeleteChain vs lookup Nothing"
                                        ++ show (i, initCm))) $
                      M.lookup i initCm


foldChains :: (Chain -> Chain) -> ChainMap -> [ChainId] -> ChainMap
foldChains adjF = foldl (flip (M.adjust adjF))
