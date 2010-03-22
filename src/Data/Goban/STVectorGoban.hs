{-# OPTIONS -O2 -Wall -Werror -Wwarn #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

{- |
   Module     : Data.Goban.Vector
   Copyright  : Copyright (C) 2010 Fabian Linzberger
   License    : GNU GPL, version 3 or above

   Maintainer : Fabian Linzberger <e@lefant.net>
   Stability  : experimental
   Portability: probably

Mutable Goban Implementation based on Data.Vector.Unboxed.Mutable

-}

module Data.Goban.STVectorGoban ( STGoban(..)
                                , newGoban
                                , copyGoban
                                , showGoban
                                , addStone
                                , deleteStones
                                , gobanSize

                                -- , isSuicideVertex
                                , isPotentialFullEye
                                -- , killedStones
                                , neighbourStones
                                , adjacentStones
                                , intAdjacentStones
                                , verticesToStones
                                , adjacentVertices
                                , intAscAdjacentVertices
                                , allStones
                                , allLibertiesColorCount
                                , colorTerritories
                                , intAllAdjacentStonesSameColor
                                ) where

import Control.Monad (filterM)
import Control.Monad.ST (ST)
import Data.Maybe (catMaybes)
import Data.Word (Word)
import Data.List ((\\), unfoldr)
import qualified Data.Vector.Unboxed.Mutable as VM
import Text.Printf (printf)

-- import Debug.TraceOrId (trace)


import Data.Goban.Types
import Data.Goban.IntVertex


data STGoban s = STGoban !Boardsize (VM.STVector s Word)


showGoban :: STGoban s -> ST s String
showGoban g@(STGoban n _v) = do
  vertexStates <- mapM (intReadGoban g) $ [0 .. (maxIntIndex n)] \\ borderVertices n
  let ls = unfoldr nLines vertexStates
  return $ board ls
    where
      board ls =
          unlines
          $ reverse
          ([xLegend]
           ++ zipWith (++) ys
              (zipWith (++) (map (unwords . (map show)) ls) ys)
           ++ [xLegend])

          where
            ys = map (printf " %2d ") [1 .. n]
            xLegend = "    " ++ unwords (map ((: []) . xToLetter) [1 .. n])
      nLines xs = if null xs then Nothing else Just $ splitAt n xs


  -- return $ (++) "\n\n" $ unlines $ reverse (xLegend : show' (1 :: Int) vertexStates)
  --   where
  --     show' _ [] = [xLegend]
  --     show' ln ls' = (ln' ++ concatMap ((" " ++) . show) left ++ ln')
  --                    : show' (ln + 1) right
  --         where
  --           (left, right) = splitAt n ls'
  --           ln' = printf "  %2d " ln

  --     xLegend = "     " ++ concatMap ((" " ++) . (: []) . xToLetter) [1 .. n]



newGoban :: Boardsize -> ST s (STGoban s)
newGoban n = do
  v <- VM.newWith (maxIntIndex n + 1) (stateToWord Empty)
  mapM_ (\i -> VM.write v i (stateToWord Border)) (borderVertices n)
  return $ STGoban n v

copyGoban :: STGoban s -> ST s (STGoban s)
copyGoban (STGoban n v) = do
  (STGoban _n v') <- newGoban n
  VM.copy v' v
  return $ STGoban n v'


addStone :: STGoban s -> Stone -> ST s ()
addStone g (Stone vertex color) =
    writeGoban g vertex (Colored color)

deleteStones :: STGoban s -> [Stone] -> ST s ()
deleteStones g =
    mapM_ ((\p -> writeGoban g p Empty) . stoneVertex)

vertexToStone :: STGoban s -> Vertex -> ST s (Maybe Stone)
vertexToStone g vertex = do
  s <- readGoban g vertex
  return $ case s of
             Colored color -> Just $ Stone vertex color
             Empty -> Nothing
             Border -> Nothing

intVertexToStone :: STGoban s -> Int -> ST s (Maybe Stone)
intVertexToStone g@(STGoban n _v) i = do
  s <- intReadGoban g i
  return $ case s of
             Colored color -> Just $ Stone (intToVertex n i) color
             Empty -> Nothing
             Border -> Nothing


writeGoban :: STGoban s -> Vertex -> VertexState -> ST s ()
writeGoban (STGoban n v) vertex state =
    VM.write v (vertexToInt n vertex) (stateToWord state)

readGoban :: STGoban s -> Vertex -> ST s VertexState
readGoban (STGoban n v) vertex =
    fmap wordToState (VM.read v (vertexToInt n vertex))


intReadGoban :: STGoban s -> Int -> ST s VertexState
intReadGoban (STGoban _n v) i =
    fmap wordToState (VM.read v i)


gobanSize :: STGoban s -> ST s Boardsize
gobanSize (STGoban n _) = return n








-- helpers: state / word conversion

stateToWord :: VertexState -> Word
stateToWord Empty = 0
stateToWord (Colored Black) = 1
stateToWord (Colored White) = 2
stateToWord Border = 3

{-# INLINE stateToWord #-}

wordToState :: Word -> VertexState
wordToState n =
    case fromIntegral n :: Int of
      0 -> Empty
      1 -> Colored Black
      2 -> Colored White
      3 -> Border
      other -> error ("wordToState parameter out of range " ++ show other)

{-# INLINE wordToState #-}







-- actual heavy lifting with go logic depending on goban data
-------------------------------------------------------------




isPotentialFullEye :: STGoban s -> Stone -> ST s Bool
isPotentialFullEye g (Stone p color) = do
  as <- mapM (readGoban g) $ adjacentVertices p
  (if all isSameColorOrBorder as
   then do
     ds <- mapM (readGoban g) $ diagonalVertices p
     let opponentDiagonalCount = length $ filter isOtherColor ds
     let borderDiagonalCount = length $ filter isBorder ds
     return ((opponentDiagonalCount == 0)
             || ((opponentDiagonalCount == 1)
                 && (borderDiagonalCount == 0)))
   else return False)

   where
     isSameColorOrBorder c = c `elem` [Border, Colored color]
     isBorder c = c == Border
     isOtherColor c = c == Colored (otherColor color)



-- isEyeLike :: STGoban -> Color -> Vertex -> Bool
-- isEyeLike goban color v =
--     (length vs == length sns)
--     && isSuicide goban (Stone (v, (otherColor color)))
--     where
--       vs = adjacentVertices goban v
--       sns = filter (\(Stone (_p', c')) -> color == c') ns
--       ns = neighbourStones goban (Stone (v, color))


-- isSuicide :: STGoban s -> Stone -> ST s Bool
-- isSuicide g stone@(Stone p c) = do
--   frees1 <- adjacentFree g p
--   (if null frees1
--    -- stone has no liberties remaining itself
--    then do
--      as <- adjacentStones g p
--      dsc <- deadStones3 g stone stone
--      (if null dsc && any (\(Stone _p color') -> c == color') as
--       -- same color neighbours reach a liberty
--       then return False
--       -- same color neighbours do not reach a liberty
--       else do
--         pdoc <- potDeadColor as (otherColor c)
--         doc <- mapM (deadStones3 g stone) pdoc >>= (return . concat)
--         (if null doc
--          -- all other color neighbours alive
--          then return True
--          -- at least one other color neighbours dies
--          else return False))
--    -- we have at least one liberty ourselves
--    else return False)
--    where
--      potDeadColor as color =
--          filterM hasOneLiberty $
--                  filter (\(Stone _p color') -> color == color') as

--      hasOneLiberty (Stone p' _c) = do
--        as <- adjacentFree g p'
--        return $ (length as == 1)


-- deadStones3 :: STGoban s -> Stone -> Stone -> ST s [Stone]
-- deadStones3 g (Stone initP _) stone@(Stone _ color) =
--     anyInMaxStringAlive [stone] []
--     where
--       anyInMaxStringAlive [] gs = return gs
--       anyInMaxStringAlive (n@(Stone p _) : ns) gs = do
--         frees <- adjacentFree g p
--         case frees of
--           [] -> do
--             hs <- liftM (filter filterF) $ genF n
--             anyInMaxStringAlive (ns ++ (((hs) \\ gs) \\ ns)) (n : gs)
--           [p'] -> if initP == p'
--                   then (do
--                          hs <- liftM (filter filterF) $ genF n
--                          anyInMaxStringAlive (ns ++ (((hs) \\ gs) \\ ns)) (n : gs))
--                   else return []
--           _ -> return []
--       genF = neighbourStones g
--       filterF (Stone _p' color') =
--           color == color'


-- isDead :: STGoban s -> Stone -> ST s Bool
-- isDead g stone =
--     deadStones g stone >>= (return . not . null)



-- -- FIXME:
-- -- if several killed neighbouring stones are part of the same
-- -- group it will be found twice here
-- -- nub at the end works around for scoring
-- killedStones :: STGoban s -> Stone -> ST s [Stone]
-- killedStones g stone@(Stone _p color) =
--     neighbourStones g stone >>=
--     filterM (return . hasOtherColor) >>=
--     mapM (deadStones g) >>= (return . nub . concat)
--     where
--       hasOtherColor (Stone _p' color') =
--           (otherColor color) == color'


-- deadStones :: STGoban s -> Stone -> ST s [Stone]
-- deadStones g stone@(Stone _p color) =
--     anyInMaxStringAlive [stone] []
--     where
--       anyInMaxStringAlive [] gs = return gs
--       anyInMaxStringAlive (n@(Stone p _color) : ns) gs = do
--           frees <- adjacentFree g p
--           (if null frees
--            then do
--                hs <- liftM (filter filterF) $ genF n
--                anyInMaxStringAlive (ns ++ (((hs) \\ gs) \\ ns)) (n : gs)
--            else return [])

--       genF = neighbourStones g
--       filterF (Stone _p' color') =
--           color == color'




neighbourStones :: STGoban s -> Stone -> ST s [Stone]
neighbourStones g (Stone p _) =
    adjacentStones g p

adjacentStones :: STGoban s -> Vertex -> ST s [Stone]
adjacentStones g p =
    verticesToStones g $ adjacentVertices p

intAdjacentStones :: STGoban s -> Int -> ST s [Stone]
intAdjacentStones g@(STGoban n _v) p =
    intVerticesToStones g $ intAscAdjacentVertices n p


verticesToStones :: STGoban s -> [Vertex] -> ST s [Stone]
verticesToStones g ps =
    fmap catMaybes (mapM (vertexToStone g) ps)

intVerticesToStones :: STGoban s -> [Int] -> ST s [Stone]
intVerticesToStones g ps =
    fmap catMaybes (mapM (intVertexToStone g) ps)
  

adjacentFree :: STGoban s -> Vertex -> ST s [Vertex]
adjacentFree g initP =
    filterM isFree $ adjacentVertices initP
    where
      isFree p = fmap (Empty ==) (readGoban g p)







allStones :: STGoban s -> ST s [Vertex]
allStones g@(STGoban n _v) =
    fmap (map stoneVertex . catMaybes)
             (mapM (intVertexToStone g)
              ([0 .. maxIntIndex n] \\ borderVertices n))


allLibertiesColorCount :: STGoban s -> Color -> ST s Int
allLibertiesColorCount g@(STGoban n _v) color = do
  stones <- mapM (intVertexToStone g) ([0 .. maxIntIndex n] \\ borderVertices n)
  liberties <- mapM (adjacentFree g . stoneVertex) $ filter sameColor $ catMaybes stones
  return $ length $ concat liberties

    where
      sameColor (Stone _p color') =
          color == color'



colorTerritories :: STGoban s -> [Int] -> ST s [(Color, [Int])]
colorTerritories g t = do
  maybeColor <- intAllAdjacentStonesSameColor g t
  return $ case maybeColor of
             Just tColor -> [(tColor, t)]
             Nothing -> []

intAllAdjacentStonesSameColor :: STGoban s -> [Int] -> ST s (Maybe Color)
intAllAdjacentStonesSameColor g ps = do
  as <- mapM (intAdjacentStones g) ps
  return $ maybeSameColor $ map stoneColor $ concat as
    where
      maybeSameColor [] = Nothing
      maybeSameColor (c : cs) =
          if all (c ==) cs
          then Just c
          else Nothing



-- groupOfStone :: STGoban s -> Stone -> ST s [Stone]
-- groupOfStone g stone@(Stone (_p, color)) =
--     maxStringM genF' filterF' stone
--     where
--       genF' = neighbourStones g
--       filterF' (Stone (_p', color')) =
--           color == color'


-- maxStringM :: (Monad m, Eq a) => (a -> m [a]) -> (a -> Bool) -> a -> m [a]
-- maxStringM genF filterF p =
--     maxString' [p] []
--     where
--       maxString' [] gs = return $ gs
--       maxString' (n : ns) gs = do
--         hs <- liftM (filter filterF) $ genF n
--         maxString' (ns ++ ((hs \\ gs) \\ ns)) (n : gs)


-- liberties :: STGoban -> [Stone] -> Int
-- liberties goban groupStones =
--     length ls
--     where
--       ls = nub ls'
--       ls' =
--           concatMap
--           (adjacentFree goban)
--           (map stoneVertex groupStones)



-- freeNonEdgeVertices :: STGoban -> [Vertex]
-- freeNonEdgeVertices goban =
--     filter (((==) Nothing) . (vertexToStone goban)) $
--            nonEdgeVertices (sizeOfGoban goban)

