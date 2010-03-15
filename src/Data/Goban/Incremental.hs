{-# OPTIONS -O2 -Wall -Werror -Wwarn #-}
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
                              , newChainGoban
                              , showChainIdGoban
                              , vertexChain
                              , addStone
                              ) where



import Control.Arrow (second)
import Control.Monad (foldM)
import Control.Monad.ST (ST)
import Data.Maybe (fromMaybe, catMaybes)
import Data.List (foldl', partition, (\\))
import Text.Printf (printf)


import qualified Data.IntMap as M
import qualified Data.Set as S
import qualified Data.Array.ST as A



-- import Debug.TraceOrId (trace)


import Data.Goban.Types
-- import Data.Goban.IntVertex


data Chain = Chain { chainId              :: !ChainId
                   , chainColor           :: !Color
                   , chainLiberties       :: !VertexSet
                   , chainVertices        :: !VertexSet
                   , chainNeighbours      :: !ChainNeighbours
                   }
           deriving (Show)

type ChainId = Int
noChainId :: ChainId
noChainId = 0

-- type LibertyCount = Int

type VertexSet = S.Set Vertex

-- maybe also reconsider Vector for this?
type ChainMap = M.IntMap Chain

-- try this instead of Vector this time
type ChainIdGoban s = A.STUArray s Vertex ChainId

type ChainNeighbours = M.IntMap VertexSet






showChainIdGoban :: ChainIdGoban s -> ST s String
showChainIdGoban cg = do
  (_, (n, _)) <- A.getBounds cg
  chainIds <- A.getElems cg
  return $ unlines $ reverse $ ([(xLegend n)] ++ (show' n (1 :: Int) chainIds))
    where
      show' :: Int -> Int -> [ChainId] -> [String]
      show' n _ [] = [xLegend n]
      show' n ln ls' = (ln' ++ concatMap ((" " ++) . show) left ++ ln')
                     : (show' n (ln + 1) right)
          where
            (left, right) = splitAt n ls'
            ln' = printf "  %2d " ln

      xLegend :: Int -> String
      xLegend n = "     " ++ concatMap (printf "%2d") [1 .. n]



newChainGoban :: Boardsize -> ST s (ChainIdGoban s)
newChainGoban n =
    A.newArray ((1, 1), (n, n)) noChainId


vertexChain :: ChainIdGoban s -> ChainMap -> Vertex -> ST s Chain
vertexChain cg cm p = do
  i <- A.readArray cg p
  return $ idChain cm i


addStone :: ChainIdGoban s -> ChainMap -> Stone -> ST s (ChainMap)
addStone cg cm s@(Stone p color) = do
  -- lookup all adjacent chain ids
  adjPs <- mapM readPairWithKey $ adjacentVertices p

  -- partition out free vertices
  (adjFreePs, adjIdPs) <- return $ partition ((== noChainId) . fst) adjPs

  -- partition friend and foe
  (ourIdPs, neighIdPs) <- return $ partition ((color ==) . chainColor . (idChain cm) . fst) adjIdPs

  -- VertexSet of adjacent liberties
  adjFrees <- return $ S.fromList $ map snd adjFreePs
  -- list of adjacent same color chain ids
  ourIds <- return $ map fst ourIdPs
  -- ChainNeighbour type neighbour id - vertex map
  neighIds <- return $ map fst neighIdPs
  neighs <- return $ M.fromListWith S.union $ map (second S.singleton) neighIdPs


  -- update neighbour chains, removing their liberties lost by current move
  -- return list of ids that as a consequence are now dead
  (cm1, deadIds) <- return $ removeNeighbourLiberties cm p neighIds

  -- delete neighbour chains that just died
  cm2 <- foldM (deleteChain cg) cm1 deadIds

  -- add new chain with played stone and
  -- merge chains becoming connected if necessary
  (cm3, i) <- return $ (let (cm3, i) = mapAddChain cm2 s adjFrees neighs in
                        case ourIds of
                          [] ->
                              (cm3, i)
                          fcs ->
                              mapFoldChains cm3 (fcs ++ [i]))


  -- write new chain id to played vertex
  A.writeArray cg p i

  return cm3

    where
      readPairWithKey ap = do
        v <- A.readArray cg ap
        return (v, ap)




deleteChain :: ChainIdGoban s -> ChainMap -> ChainId -> ST s (ChainMap)
deleteChain cg cm i = do
  mapM_ resetVertex vs
  return $ mapDeleteChain cm i
  where
    resetVertex p = A.writeArray cg p noChainId
    vs = S.elems $ chainVertices $ idChain cm i











idChain :: ChainMap -> ChainId -> Chain
idChain cm i =
    fromMaybe (error "idChain Nothing") $ M.lookup i cm

nextChainId :: ChainMap -> ChainId
nextChainId cm = head ([1 ..] \\ M.keys cm)



removeNeighbourLiberties :: ChainMap -> Vertex -> [ChainId]
                         -> (ChainMap, [ChainId])
removeNeighbourLiberties initCm p neighIds =
    (cm', dead)
    where
      dead = filter isDeadChain neighIds

      isDeadChain :: ChainId -> Bool
      isDeadChain i = S.null $ chainLiberties $ idChain cm' i


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
      c = Chain { chainId = i
                , chainColor = color
                , chainLiberties = adjFrees
                , chainVertices = S.singleton p
                , chainNeighbours = neighs
                }
      i = nextChainId cm

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


mapFoldChains :: ChainMap -> [ChainId] -> (ChainMap, ChainId)
mapFoldChains cm ois@(i : is) =
    (cm'', i)
    where
      cm'' = mapFoldChainsNeighbours cm' ois neighs
      cm' = M.insert i c' cm
      c' = c { chainLiberties = S.unions $ map chainLiberties cs
             , chainVertices = S.unions $ map chainVertices cs
             , chainNeighbours = neighs
             }
      neighs = M.unionsWith S.union $ map chainNeighbours cs
      cs = c : (map (idChain cm) is)
      c = idChain cm i
mapFoldChains _ [] = error "mapFoldChains called with empty list"

mapFoldChainsNeighbours :: ChainMap -> [ChainId] -> ChainNeighbours -> ChainMap
mapFoldChainsNeighbours initCm (i : is) neighs =
    foldl' updateCM initCm $ M.keys neighs
    where
      updateCM :: ChainMap -> ChainId -> ChainMap
      updateCM cm ni = M.adjust updateNC ni cm

      updateNC :: Chain -> Chain
      updateNC c =
          c { chainNeighbours = nNeighs' }
          where
            nNeighs' = M.adjust (l `S.union`) i nNeighs
            l = S.unions $ catMaybes $ map (\j -> M.lookup j nNeighs) is
            nNeighs = chainNeighbours c
mapFoldChainsNeighbours _ [] _ = error "mapFoldChainsNeighbours called with empty list"


mapDeleteChain :: ChainMap -> ChainId -> ChainMap
mapDeleteChain initCm i =
    M.delete i
         $ foldl' updateCM initCm
               $ M.assocs
                     $ chainNeighbours
                           $ fromMaybe (error "mapDeleteChain Nothing")
                                 $ M.lookup i initCm
    where
      updateCM :: ChainMap -> (ChainId, VertexSet) -> ChainMap
      updateCM cm (nId, vs) =
          M.adjust (updateNC vs) nId cm
      updateNC :: VertexSet -> Chain -> Chain
      updateNC vs c =
          c { chainLiberties = chainLiberties c `S.union` vs
            , chainNeighbours = M.delete i $ chainNeighbours c
            }









-- allStones :: STGoban s -> ST s [Vertex]
-- allStones g@(STGoban n _v) = do
--   mapM (intVertexToStone g) ([0 .. (maxIntIndex n)] \\ (borderVertices n)) >>= (return . (map stoneVertex) . catMaybes)


-- allLibertiesColorCount :: STGoban s -> Color -> ST s Int
-- allLibertiesColorCount g@(STGoban n _v) color = do
--   stones <- mapM (intVertexToStone g) ([0 .. (maxIntIndex n)] \\ (borderVertices n))
--   liberties <- mapM (adjacentFree g) $ map stoneVertex $ filter sameColor $ catMaybes stones
--   return $ length $ concat liberties

--     where
--       sameColor (Stone _p color') =
--           color == color'
