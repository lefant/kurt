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
                              , ChainMap
                              , ChainIdGoban
                              , ChainIdGobanFrozen

                              , isSuicide
                              , stonesAndLiberties

                              , newChainGoban
                              , newChainMap
                              , showChainIdGoban
                              , vertexChain
                              , addChainStone
                              ) where



import Control.Arrow (second)
import Control.Monad (foldM)
import Control.Monad.ST (ST)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.List (foldl', partition, unfoldr, transpose, nub, sort)
import Text.Printf (printf)


import qualified Data.IntMap as M
import qualified Data.Set as S
import qualified Data.Array.ST as STUA
import qualified Data.Array.Unboxed as UA


-- import Debug.TraceOrId (trace)


import Data.Goban.Types
-- import Data.Goban.IntVertex


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

-- type LibertyCount = Int

type VertexSet = S.Set Vertex

-- maybe also reconsider Vector for this?
type ChainMap = M.IntMap Chain

showChainMap :: ChainMap -> String
showChainMap cm =
    concatMap (\(k, v) -> show k ++ " " ++ show v ++ "\n") $ M.toList cm


-- try this instead of Vector this time
type ChainIdGoban s = STUA.STUArray s Vertex ChainId

type ChainIdGobanFrozen = UA.UArray Vertex ChainId

type ChainNeighbours = M.IntMap VertexSet






isSuicide :: ChainIdGoban s -> ChainMap -> Stone -> ST s Bool
isSuicide cg cm s@(Stone p _color) = do
  (adjFrees, ourIds, neighIds, _neighs) <- adjacentStuff cg cm s

  return (S.null adjFrees
          && all (S.null . S.delete p . chainLiberties . idChain "isSuicide ourIds" cm) ourIds
          && all (not . S.null . S.delete p . chainLiberties . idChain "isSuicide neighIds" cm) neighIds)




stonesAndLiberties :: ChainIdGobanFrozen -> ChainMap -> Stone
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
      adjPs = map readPairWithKey $ adjacentVerticesInBounds n p

      readPairWithKey ap =
          (v, ap)
          where
            v = cg UA.! ap

      (_, (n, _)) = UA.bounds cg











showChainIdGoban :: ChainIdGoban s -> ST s String
showChainIdGoban cg = do
  (_, (n, _)) <- STUA.getBounds cg
  chainIds <- STUA.getElems cg
  let ls = transpose $ unfoldr (nLines n) chainIds
  return $ board n ls
    where
      board n ls =
          unlines
          $ reverse
          ([xLegend]
           ++ zipWith (++) ys
              (zipWith (++) (map (concatMap maybePrint) ls) ys)
           ++ [xLegend])

          where
            ys = map (printf " %2d ") [1 .. n]
            xLegend = "    " ++ unwords (map ((: []) . xToLetter) [1 .. n])

      maybePrint 0 = " ."
      maybePrint m = printf "%2d" m
      nLines n xs = if null xs then Nothing else Just $ splitAt n xs


newChainGoban :: Boardsize -> ST s (ChainIdGoban s)
newChainGoban n =
    STUA.newArray ((1, 1), (n, n)) noChainId


vertexChain :: ChainIdGoban s -> ChainMap -> Vertex -> ST s Chain
vertexChain cg cm p = do
  i <- STUA.readArray cg p
  return $ idChain "vertexChain" cm i


addChainStone :: ChainIdGoban s -> ChainMap -> Stone -> ST s (ChainMap, [Stone])
addChainStone cg cm s@(Stone p _color) = do
  (adjFrees, ourIds, neighIds, neighs) <- adjacentStuff cg cm s

  -- add new chain with played stone and
  -- merge chains becoming connected if necessary
  (cm3, i) <- (let (cm1, j) = mapAddChain cm s adjFrees neighs in
               case ourIds of
                 [] ->
                     return (cm1, j)
                 js ->
                     do
                       is@(i : _) <- return $ sort (j : js)
                       let cm2 = mapFoldChains cm1 is p
                       mapM_ (\p' -> STUA.writeArray cg p' i)
                        $ S.elems $ chainVertices
                              $ idChain "addStone merge" cm2 i
                       return (cm2, i))


  -- update neighbour chains, removing their liberties lost by current move
  -- return list of ids that as a consequence are now dead
  (cm4, deadIds) <- return $ removeNeighbourLiberties cm3 p neighIds

  let dead = deadStones deadIds

  -- delete neighbour chains that just died
  cm5 <- foldM (deleteChain cg) cm4 deadIds


  -- write new chain id to played vertex
  STUA.writeArray cg p i

  -- trace ("after addStone\n" ++ showChainMap cm5) $ return ()

  return (cm5, dead)

    where
      deadStones :: [ChainId] -> [Stone]
      deadStones =
          concatMap (chainStones . idChain "deadStones" cm)

      chainStones :: Chain -> [Stone]
      chainStones c =
          map (flip Stone (chainColor c)) $ S.toList $ chainVertices c



adjacentStuff :: ChainIdGoban s -> ChainMap -> Stone
              -> ST s (VertexSet,
                       [ChainId],
                       [ChainId],
                       ChainNeighbours)
adjacentStuff cg cm (Stone p color) = do
  (_, (n, _)) <- STUA.getBounds cg

  -- lookup all adjacent chain ids
  adjPs <- mapM readPairWithKey $ adjacentVerticesInBounds n p

  -- partition out free vertices
  (adjFreePs, adjIdPs) <- return $ partition ((== noChainId) . fst) adjPs

  -- partition friend and foe
  (ourIdPs, neighIdPs) <- return $ partition ((color ==) . chainColor . idChain ("adjacentStuff partition " ++ show adjIdPs ++ "\n" ++ showChainMap cm) cm . fst) adjIdPs

  -- VertexSet of adjacent liberties
  let adjFrees = S.fromList $ map snd adjFreePs
  -- list of adjacent same color chain ids
  let ourIds = nub $ map fst ourIdPs
  -- ChainNeighbour type neighbour id - vertex map
  let neighIds = nub $ map fst neighIdPs
  let neighs = M.fromListWith S.union $ map (second S.singleton) neighIdPs


  return (adjFrees, ourIds, neighIds, neighs)

    where
      readPairWithKey ap = do
        v <- STUA.readArray cg ap
        return (v, ap)



deleteChain :: ChainIdGoban s -> ChainMap -> ChainId -> ST s ChainMap
deleteChain cg cm i = do
  mapM_ resetVertex vs
  return $ mapDeleteChain cm i
  where
    resetVertex p = STUA.writeArray cg p noChainId
    vs = S.elems $ chainVertices $ idChain ("deleteChain " ++ show i) cm i









newChainMap :: ChainMap
newChainMap = M.empty

idChain :: String -> ChainMap -> ChainId -> Chain
idChain caller cm i =
    -- trace ("idChain " ++ show (i, cm)) $
    fromMaybe (error ("idChain Nothing " ++ caller)) $ M.lookup i cm

nextChainId :: ChainMap -> ChainId
nextChainId cm =
    if null ks then 1 else succ $ last ks
    where
      ks = M.keys cm




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
