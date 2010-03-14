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
                              , deleteChain
                              , mapDeleteChain
                              , vertexChain
                              ) where



-- import Control.Monad (liftM, filterM)
import Control.Monad.ST (ST)
import Data.Maybe (fromMaybe, catMaybes)
-- import Data.Word (Word)
import Data.List (foldl', sort, group, partition, (\\))
import Text.Printf (printf)


import qualified Data.IntMap as M
import qualified Data.Set as S
import qualified Data.Array.ST as A



-- import Debug.TraceOrId (trace)


import Data.Goban.Types
import Data.Goban.IntVertex


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



addStone :: ChainIdGoban s -> ChainMap -> Stone -> ST s (ChainMap)
addStone cg cm s@(Stone p color) = do
  (fvs, ours, cns) <- adjacentStuff cg cm s

  (cm', i) <- return $ case ours of
                         [] ->
                             mapAddChain cm s fvs cns
                         fcs ->
                             (mapFoldChains cm' (i : fcs), i)
                             where
                               (cm', i) = mapAddChain cm s fvs cns
  A.writeArray cg p i

  case cns of
    _ -> return ()

  return cm'
    

adjacentStuff :: ChainIdGoban s -> ChainMap -> Stone
              -> ST s (VertexSet, [ChainId], ChainNeighbours)
adjacentStuff cg cm (Stone initP color) = do
    as <- mapM m $ adjacentVertices initP
    (ls, acs) <- return $ partition ((== noChainId) . fst) as
    (ours, others) <- return $ partition ((color ==) . chainColor . (idChain cm) . fst) acs

    return $ (fvs ls, map fst ours, cns others)
    where
      fvs ls = S.fromList $ map snd ls

      cns others = M.fromListWith S.union
             $ map (\(k, v) -> (k, S.singleton v)) others

      m p = do
        v <- A.readArray cg p
        return (v, p)


deleteChain :: ChainIdGoban s -> ChainMap -> ChainId -> ST s (ChainMap)
deleteChain cg cm i = do
  mapM_ resetVertex vs
  return $ mapDeleteChain cm i
  where
    resetVertex p = A.writeArray cg p noChainId
    vs = S.elems $ chainVertices $ idChain cm i



vertexChain :: ChainIdGoban s -> ChainMap -> Vertex -> ST s Chain
vertexChain cg cm p = do
  i <- A.readArray cg p
  return $ idChain cm i








idChain :: ChainMap -> ChainId -> Chain
idChain cm i =
    fromMaybe (error "idChain Nothing") $ M.lookup i cm

nextChainId :: ChainMap -> ChainId
nextChainId cm = head ([1 ..] \\ M.keys cm)



mapAddChain :: ChainMap -> Stone -> VertexSet -> ChainNeighbours -> (ChainMap, ChainId)
mapAddChain cm s@(Stone p color) ls ecs =
    (cm', i)
    where
      cm' = M.insert i c cm
      c = Chain { chainId = i
                , chainColor = color
                , chainLiberties = ls
                , chainVertices = S.singleton p
                , chainNeighbours = ecs
                }
      i = nextChainId cm

-- mapAddToChain :: ChainMap -> ChainId -> Stone -> [Vertex] -> [(ChainId, Vertex)] -> ChainMap
-- mapAddToChain cm i s@(Stone p color) fs ecs =
--     M.adjust updateChain i cm
--     where
--       updateChain :: Chain -> Chain
--       updateChain c =
--           c { chainLiberties = chainLiberties c `S.union` S.fromList fs
--             , chainVertices = S.insert p $ chainVertices c
--             , chainNeighbours = error "mapAddToChain FIXME: TODO"
--             }

mapFoldChains :: ChainMap -> [ChainId] -> ChainMap
mapFoldChains cm ois@(i : is) =
    mapFoldChainsNeighbours cm' ois (M.keys cns)
    where
      cm' = M.insert i c' cm
      c' = c { chainLiberties = S.unions $ map chainLiberties cs
             , chainVertices = S.unions $ map chainVertices cs
             , chainNeighbours = cns
             }
      cns = M.unionsWith S.union $ map chainNeighbours cs
      cs = c : (map (idChain cm) is)
      c = idChain cm i
mapFoldChains _ _ = error "mapFoldChains called with single ChainId"


mapFoldChainsNeighbours :: ChainMap -> [ChainId] -> [ChainId] -> ChainMap
mapFoldChainsNeighbours initCm (i : is) ncs =
    foldl' updateCM initCm ncs
    where
      updateCM :: ChainMap -> ChainId -> ChainMap
      updateCM cm ni = M.adjust updateNC ni cm

      updateNC :: Chain -> Chain
      updateNC c =
          c { chainNeighbours = ncnm' }
          where
            ncnm' = M.adjust (l `S.union`) i ncnm
            l = S.unions $ catMaybes $ map (\j -> M.lookup j ncnm) is
            ncnm = chainNeighbours c
mapFoldChainsNeighbours _ _ _ = error "mapFoldChainsNeighbours called with single ChainId"


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
      updateCM cm (nId, ls) =
          M.adjust (updateNC ls) nId cm
      updateNC :: VertexSet -> Chain -> Chain
      updateNC ls c =
          c { chainLiberties = chainLiberties c `S.union` ls
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
