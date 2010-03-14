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
                              ) where



import Control.Monad (liftM, filterM)
import Control.Monad.ST (ST)
-- import Data.Maybe (catMaybes)
-- import Data.Word (Word)
-- import Data.List ((\\), nub)
import Text.Printf (printf)


import qualified Data.IntMap as M
import qualified Data.IntSet as S
import qualified Data.Array.ST as A



import Debug.TraceOrId (trace)


import Data.Goban.Types
import Data.Goban.IntVertex


data Chain = Chain { chainId        :: !ChainId
                   , chainColor     :: !Color
                   , chainLiberties :: !LibertyCount
                   , chainVertices  :: ![Vertex]
                   }
           deriving (Show)

type ChainId = Int

type LibertyCount = Int


-- maybe also reconsider Vector for this?
type ChainMap = M.IntMap Chain

-- try this instead of Vector this time
type ChainIdGoban s = A.STUArray s Vertex ChainId




showChainIdGoban :: ChainIdGoban s -> String
showChainIdGoban cg = do
  (_, (n, _)) <- A.getBounds cg
  chainIds <- A.getElems cg
  return $ (++) "\n\n" $ unlines $ reverse $ ((xLegend n) : (show' n (1 :: Int) chainIds))
    where
      show' n _ [] = [xLegend n]
      show' n ln ls' = (ln' ++ concatMap ((" " ++) . show) left ++ ln')
                     : (show' n (ln + 1) right)
          where
            (left, right) = splitAt n ls'
            ln' = printf "  %2d " ln

      xLegend n = "     " ++ concatMap (printf "%2d") [1 .. n]



newGoban :: Boardsize -> ST s (ChainIdGoban s)
newGoban n =
    A.newArray ((1, 1), (n, n)) (0 :: ChainId)


vertexChain :: ChainIdGoban s -> ChainMap -> Vertex -> ST s (Maybe Chain)
vertexChain cg cm p = do
    i <- A.readArray cg p
    return $ M.lookup i cm
      



-- addStone :: STGoban s -> Stone -> ST s ()
-- addStone g (Stone vertex color) =
--     writeGoban g vertex (Colored color)

-- deleteStones :: STGoban s -> [Stone] -> ST s ()
-- deleteStones g stones =
--     mapM_ (\p -> writeGoban g p Empty) $ map stoneVertex stones




-- do
--   s <- readGoban g vertex
--   return $ case s of
--              Colored color -> Just $ Stone vertex color
--              Empty -> Nothing
--              Border -> Nothing

-- intVertexToStone :: STGoban s -> Int -> ST s (Maybe Stone)
-- intVertexToStone g@(STGoban n _v) i = do
--   s <- intReadGoban g i
--   return $ case s of
--              Colored color -> Just $ Stone (intToVertex n i) color
--              Empty -> Nothing
--              Border -> Nothing


-- writeGoban :: STGoban s -> Vertex -> VertexState -> ST s ()
-- writeGoban (STGoban n v) vertex state =
--     VM.write v (vertexToInt n vertex) (stateToWord state)

-- readGoban :: STGoban s -> Vertex -> ST s VertexState
-- readGoban (STGoban n v) vertex =
--     VM.read v (vertexToInt n vertex) >>= (return . wordToState)

-- intReadGoban :: STGoban s -> Int -> ST s VertexState
-- intReadGoban (STGoban _n v) i =
--     VM.read v i >>= (return . wordToState)


-- gobanSize :: STGoban s -> ST s Boardsize
-- gobanSize (STGoban n _) = return n







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
