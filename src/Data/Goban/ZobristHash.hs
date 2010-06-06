{-# OPTIONS -O2 -Wall -Werror -Wwarn #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

{- |
   Module     : Data.Goban.ZobristHash
   Copyright  : Copyright (C) 2010 Fabian Linzberger
   License    : GNU GPL, version 3 or above

   Maintainer : Fabian Linzberger <e@lefant.net>
   Stability  : experimental
   Portability: probably

Zobrist hashing for go.

-}

module Data.Goban.ZobristHash ( Hash
                              , HashMap
                              , initMap
                              , updateHash
                              ) where


import Data.Maybe (fromJust)
import Data.Map as M (Map, empty, insert, lookup)
import Data.Bits (xor)
import System.Random (randomIO)
import Control.Monad (foldM)

import Data.Goban.Types

-- type Hash = Int64
type Hash = Int
type VState = (Vertex, VertexState)
type HashMap = M.Map VState Hash



initMap :: Int -> IO HashMap
initMap n =
    foldM f M.empty xs
    where
      xs = [(v, s) | v <- vs, s <- [Colored Black, Colored White, EmptyKoBlocked]]
      vs = allVertices n

      f :: HashMap -> VState -> IO HashMap
      f m x = do
        y <- randomIO
        return $ M.insert x y m
          
updateHash :: HashMap -> Hash -> VState -> Hash
updateHash m h v =
    h `xor` (fromJust $ M.lookup v m)
