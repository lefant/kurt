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
                              , vertexChain
                              , addStone
                              ) where



import Control.Arrow (second)
import Control.Monad (liftM, foldM)
import Control.Monad.ST (ST)
import Data.Maybe (fromMaybe, mapMaybe, catMaybes)
import Data.List (foldl', partition, unfoldr, transpose, nub, sort)
import Text.Printf (printf)


import qualified Data.IntMap as M
import qualified Data.Set as S

import qualified Data.HashMap.Strict as H

-- import Debug.TraceOrId (trace)


import Data.Goban.Types


data Chain = Chain { chainColor           :: !Color
                   , chainLiberties       :: !VertexSet
                   , chainVertices        :: !VertexSet
                   , chainNeighbours      :: !ChainNeighbours
                   }

instance Show Chain where
    show (Chain c ls vs ns) =
        show c ++ " vs:" ++ unwords (map gtpShowVertex (S.elems vs))
               ++ " ls:" ++ unwords (map gtpShowVertex (S.elems ls))
               ++ "\nns: "
               ++ concatMap (\(k, v) -> show k ++ " "
                              ++ unwords (map gtpShowVertex (S.elems v))
                              ++ "; ") (M.toList ns)
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

-- should really be M.IntMap ChainId, why duplicate all groups as neighbours?!?
type ChainNeighbours = M.IntMap VertexSet






isPotentialFullEye :: GobanMap -> Stone -> Bool
isPotentialFullEye g (Stone p color) =
  if all isSameColorOrBorder as
  then (opponentDiagonalCount == 0) ||
       ((opponentDiagonalCount == 1) && (borderDiagonalCount == 0))
  else False
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
verticesToStones g ps = fmap catMaybes $ map (vertexStone g) ps





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

      (cm3, deadIds) = removeNeighbourLiberties cm2 p neighIds

      cm2 = case ourIds of
              [] -> cm1
              js ->
                  mapFoldChains cm1 (sort (j : js)) p
              where
                (cm1, j) = mapAddChain cm s adjFrees neighs


      adjFrees = S.fromList $ map snd adjFreePs
      -- list of adjacent same color chain ids
      ourIds = nub $ map fst ourIdPs
      -- ChainNeighbour type neighbour id - vertex map
      neighIds = nub $ map fst neighIdPs
      neighs = M.fromListWith S.union $ map (second S.singleton) neighIdPs

      -- partition friend and foe
      (ourIdPs, neighIdPs) = partition ((color ==) . chainColor . idChain "stonesAndLiberties partition " cm . fst) adjIdPs

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
    unlines $ reverse
                ([xLegend]
                 ++ zipWith (++) ys
                        (zipWith (++) (map (unwords . (map show)) ls) ys)
                 ++ [xLegend])
    where
      ls = transpose $ unfoldr nLines $ map idState chainIds
      nLines xs = if null xs then Nothing else Just $ splitAt n xs
      ys = map (printf " %2d ") [1 .. n]
      xLegend = "    " ++ unwords (map ((: []) . xToLetter) [1 .. n])
      n = n1 - 1
      chainIds = filter (/= borderChainId) $ map lookupVertex $ allVertices n1
      lookupVertex v = vertexId goban v
      (n1, _) = maximum $ H.keys goban


isSuicide :: GobanMap -> ChainMap -> Stone -> Bool
isSuicide cg cm (Stone p color) =
  isSuicide' adjPs
  where
    adjPs = filter (/= borderChainId) $ map vertexId $ adjacentVertices p
    isSuicide' adjPs =
      (null adjFrees
       && all (S.null . S.delete p . chainLiberties . idChain "isSuicide ourIds" cm) ourIds'
       && all (not . S.null . S.delete p . chainLiberties . idChain "isSuicide neighIds" cm) neighIds')
      where
        -- partition out free vertices
        (adjFrees, adjIds) = partition (== noChainId) adjPs
        -- partition friend and foe
        (ourIds, neighIds) = partition ((color ==) . chainColor . idChain ("adjacentStuff partition " ++ show adjIds ++ "\n" ++ showChainMap cm) cm) adjIds
        -- list of adjacent same color chain ids
        ourIds' = nub ourIds
        -- ChainNeighbour type neighbour id - vertex map
        neighIds' = nub neighIds




addStone :: GobanMap s -> ChainMap -> Stone -> ST s (ChainMap, [Stone])
addStone cg cm s@(Stone p color) = do
  -- lookup all adjacent chain ids
  adjPs <- liftM (filter ((/= borderChainId) . fst)) $ mapM readPairWithKey $ adjacentVertices p

  let (adjFrees, ourIds, neighIds, neighs) = addStone' adjPs

  -- add new chain with played stone and
  -- merge chains becoming connected if necessary
  (cm3, i) <- (let (cm1, j) = mapAddChain cm s adjFrees neighs in
               case ourIds of
                 [] ->
                     return (cm1, j)
                 js ->
                     do
                       let is@(i : _) = sort (j : js)
                       let cm2 = mapFoldChains cm1 is p
                       mapM_ (\p' -> STUA.writeArray cg p' i)
                        $ S.elems $ chainVertices
                              $ idChain "addStone merge" cm2 i
                       return (cm2, i))

  -- update neighbour chains, removing their liberties lost by current move
  -- return list of ids that as a consequence are now dead
  let (cm4, deadIds) = removeNeighbourLiberties cm3 p neighIds

  let dead = deadStones deadIds


  -- delete neighbour chains that just died
  cm5 <- foldM (deleteChain cg) cm4 deadIds

  -- write new chain id to played vertex
  STUA.writeArray cg p i

  -- trace ("after addStone\n" ++ showChainMap cm5) $ return ()

  return (cm5, dead)

    where
      addStone' adjPs =
          (adjFrees, ourIds, neighIds, neighs)
          where
            -- partition out free vertices
            (adjFreePs, adjIdPs) = partition ((== noChainId) . fst) adjPs

            -- partition friend and foe
            (ourIdPs, neighIdPs) = partition ((color ==) . chainColor . idChain ("adjacentStuff partition " ++ show adjIdPs ++ "\n" ++ showChainMap cm) cm . fst) adjIdPs

            -- VertexSet of adjacent liberties
            adjFrees = S.fromList $ map snd adjFreePs
            -- list of adjacent same color chain ids
            ourIds = nub $ map fst ourIdPs
            -- ChainNeighbour type neighbour id - vertex map
            neighIds = nub $ map fst neighIdPs
            -- neighs
            neighs = M.fromListWith S.union $ map (second S.singleton) neighIdPs


      readPairWithKey ap = do
        v <- STUA.readArray cg ap
        return (v, ap)


      deadStones :: [ChainId] -> [Stone]
      deadStones =
          concatMap (chainStones . idChain "deadStones" cm)

      chainStones :: Chain -> [Stone]
      chainStones c =
          map (flip Stone (chainColor c)) $ S.toList $ chainVertices c





deleteChain :: GobanMap -> ChainMap -> ChainId -> (ChainMap, GobanMap)
deleteChain cg cm i =
  (cm', cg')
  where
    cm' = mapDeleteChain cm i
    cg' = foldl (flip H.delete) cg vs
    vs = S.elems $ chainVertices $ idChain ("deleteChain " ++ show i) cm i





vertexChain :: GobanMap -> ChainMap -> Vertex -> Chain
vertexChain cg cm p = idChain "vertexChain" cm $ vertexId cg p

vertexStone :: GobanMap -> Vertex -> Maybe Stone
vertexStone cg p =
  case vertexState cg p of
    Colored color -> Just $ Stone p color
    Empty -> Nothing
    EmptyKoBlocked -> Nothing
    Border -> Nothing

vertexState :: GobanMap -> Vertex -> VertexState
vertexState = idState . vertexId

vertexId :: GobanMap -> Vertex -> Int
vertexId cg p = H.lookupDefault noChainId p cg






newChainMap :: ChainMap
newChainMap = M.empty

idChain :: String -> ChainMap -> ChainId -> Chain
idChain caller cm i =
    -- trace ("idChain " ++ show (i, cm)) $
    fromMaybe (error ("idChain Nothing " ++ caller)) $ M.lookup i cm

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



removeNeighbourLiberties :: ChainMap -> Vertex -> [ChainId]
                         -> (ChainMap, [ChainId])
removeNeighbourLiberties initCm p neighIds =
    (cm', dead)
    where
      dead = nub $ filter isDeadChain neighIds

      isDeadChain :: ChainId -> Bool
      isDeadChain i = S.null $ chainLiberties $ idChain "isDeadChain" cm' i

      cm' = foldl' updateCM initCm neighIds

      updateCM :: ChainMap -> ChainId -> ChainMap
      updateCM cm i = M.adjust updateNC i cm

      updateNC :: Chain -> Chain
      updateNC c =
          c { chainLiberties = S.delete p $ chainLiberties c }



mapAddChain :: ChainMap -> Stone -> VertexSet -> ChainNeighbours
            -> (ChainMap, ChainId)
mapAddChain cm (Stone p color) adjFrees neighs =
    (cm'' , i)
    where
      cm'' = mapAddChainNeighbours cm' i (S.singleton p) neighs
      cm' = M.insert i c cm
      c = Chain { chainColor = color
                , chainLiberties = adjFrees
                , chainVertices = S.singleton p
                , chainNeighbours = neighs
                }
      i = nextChainId cm color

mapAddChainNeighbours :: ChainMap -> ChainId -> VertexSet -> ChainNeighbours
                      -> ChainMap
mapAddChainNeighbours initCm i vs neighs =
    foldl' updateCM initCm $ M.keys neighs
    where
      updateCM :: ChainMap -> ChainId -> ChainMap
      updateCM cm ni = M.adjust updateNC ni cm

      updateNC :: Chain -> Chain
      updateNC c =
          c { chainNeighbours = M.insert i vs $ chainNeighbours c }


mapFoldChains :: ChainMap -> [ChainId] -> Vertex -> ChainMap
mapFoldChains cm ois@(i : is) p =
    -- trace ("mapFoldChains cm''': " ++ show cm''')
    cm'''
    where
      cm''' = mapFoldChainsNeighbours cm'' ois neighs
      cm'' = M.insert i c' cm'
      cm' = foldl' (flip M.delete) cm is
      c' = c { chainLiberties = S.delete p $ S.unions $ map chainLiberties cs
             , chainVertices = S.unions $ map chainVertices cs
             , chainNeighbours = neighs
             }
      neighs = M.unionsWith S.union $ map chainNeighbours cs
      cs = c : map (idChain "mapFoldChains2" cm) is
      c = idChain "mapFoldChains1" cm i
mapFoldChains _ [] _ = error "mapFoldChains called with empty list"

mapFoldChainsNeighbours :: ChainMap -> [ChainId] -> ChainNeighbours
                        -> ChainMap
mapFoldChainsNeighbours initCm (i : is) neighs =
    foldl' updateCM initCm $ M.keys neighs
    where
      updateCM :: ChainMap -> ChainId -> ChainMap
      updateCM cm ni = M.adjust updateNC ni cm

      updateNC :: Chain -> Chain
      updateNC c =
          c { chainNeighbours = nNeighs''' }
          where
            nNeighs''' = foldl' (flip M.delete) nNeighs'' is
            nNeighs'' = M.adjust (nvs `S.union`) i nNeighs'
            nNeighs' = M.insertWith S.union i S.empty nNeighs
            nvs = S.unions $ mapMaybe (`M.lookup` nNeighs) is
            nNeighs = chainNeighbours c
mapFoldChainsNeighbours _ [] _ = error "mapFoldChainsNeighbours called with empty list"


mapDeleteChain :: ChainMap -> ChainId -> ChainMap
mapDeleteChain initCm i =
    M.delete i
         $ foldl' updateCM initCm
               $ M.keys $ chainNeighbours
                     $ idChain "mapDeleteChain" initCm i
    where
      updateCM :: ChainMap -> ChainId -> ChainMap
      updateCM cm nId =
          M.adjust updateNC nId cm
      updateNC :: Chain -> Chain
      updateNC c =
          c { chainLiberties = chainLiberties c `S.union` vs
            , chainNeighbours = M.delete i neighs
            }
          where
            vs = fromMaybe (error ("mapDeleteChain vs lookup Nothing"
                            ++ show (i, neighs)))
                 $ M.lookup i neighs
            neighs = chainNeighbours c

