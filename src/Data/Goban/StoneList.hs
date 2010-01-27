{-# OPTIONS -O2 -Wall -Werror -Wwarn #-}

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
   Module     : Data.Goban.StoneList
   Copyright  : Copyright (C) 2010 Fabian Linzberger
   License    : GNU GPL, version 3 or above

   Maintainer : Fabian Linzberger <e@lefant.net>
   Stability  : experimental
   Portability: probably

Goban Implementation using a list of (Vertex, Color) tuples. Looking
up status of a vertex using toStone is slow, because it needs to scan
the whole list.

-}

module Data.Goban.StoneList (
                             StoneListGoban(..)
                            ,freeVertices
                            ,isSuicide
                            ,neighbourStones
                            ) where

-- import Data.List
-- import Data.Maybe
import Data.Goban.Utils
-- import Debug.Trace (trace)


newtype StoneListGoban = StoneListGoban (Int, [Stone])

instance Goban StoneListGoban where
    -- freeVertices :: a -> [Vertex]
    freeVertices _a = undefined
    -- isSuicide :: a -> Stone -> Bool
    isSuicide _a _s = undefined
    -- neighbourStones :: a -> Stone -> [Stone]
    neighbourStones _a _s = undefined
    -- groupOfStone :: a -> Stone -> [Stone]
    groupOfStone _a _s = undefined
    -- territory :: a -> Color -> Score
    territory _a _c = undefined


-- isSuicide :: Int -> Stone -> [Stone] -> Bool
-- isSuicide bsize stone allStones =
--     isDead bsize stone stones''
--     where
--       dead = deadStones bsize stone stones'
--       stones' = (stone : allStones)
--       stones'' = stones' \\ dead

-- isDead :: Int -> Stone -> [Stone] -> Bool
-- isDead bsize stone allStones =
--     -- trace ("isDead called with: " ++ (show stone) ++ " liberties: " ++ (show libertyCount))
--     libertyCount == 0
--     where
--       libertyCount = liberties bsize (groupOfStone bsize stone allStones) allStones


-- -- FIXME:
-- -- if several killed neighbouring stones are part of the same
-- -- group it will be found twice here
-- -- nub at the end works around for scoring
-- deadStones :: Int -> Stone -> [Stone] -> [Stone]
-- deadStones bsize stone@(Stone (_p, color)) allStones =
--     nub $ concatMap dead_stones' ns
--     where
--       dead_stones' n =
--           if liberties bsize groupStones allStones == 0
--           then groupStones
--           else []
--           where
--             groupStones = groupOfStone bsize n allStones

--       ns = filter hasOtherColor $ neighbourStones bsize allStones stone

--       hasOtherColor (Stone (_p', color')) =
--           (otherColor color) == color'



-- liberties :: Int -> [Stone] -> [Stone] -> Int
-- liberties bsize groupStones allStones =
--     length ls
--     where
--       ls = nub ls'
--       ls' = concatMap (adjacentFree bsize allStones) groupVertices
--       groupVertices = (verticesFromStones groupStones)


-- territory :: Int -> [Stone] -> Color -> Score
-- territory bsize allStones color =
--     sum $ map (fromIntegral . length)
--             $ filter f (emptyStrings bsize allStones)
--     where
--       f :: [Vertex] -> Bool
--       f gs =
--           all (((==) color) . stoneColor)
--                   $ concatMap (adjacentStones bsize allStones) gs


-- emptyStrings :: Int -> [Stone] -> [[Vertex]]
-- emptyStrings bsize allStones =
--     emptyStrings' empties []
--     where
--       empties = (freeVertices bsize allStones)

--       emptyStrings' [] gs = gs
--       emptyStrings' (a : as) gs =
--           emptyStrings' (as \\ ma) (ma : gs)
--           where
--             ma = maxEmptyString a

--       maxEmptyString = maxString (adjacentVertices bsize) isEmptyVertex

--       isEmptyVertex v = (toStone allStones v) == Nothing



-- maxString :: (Eq a) => (a -> [a]) -> (a -> Bool) -> a -> [a]
-- maxString genF filterF p =
--     maxString' [p] []
--     where
--       maxString' [] gs = gs
--       maxString' (n : ns) gs =
--           maxString' (ns ++ (((fgen n) \\ gs) \\ ns)) (n : gs)
--       fgen n =
--           filter filterF $ genF n


-- groupOfStone :: Int -> Stone -> [Stone] -> [Stone]
-- groupOfStone bsize stone@(Stone (_p, color)) allStone =
--     maxString genF' filterF' stone
--     where
--       genF' stone' = neighbourStones bsize allStone stone'
--       filterF' (Stone (_p', color')) =
--           color == color'

-- neighbourStones :: Int -> [Stone] -> Stone -> [Stone]
-- neighbourStones bsize allStones (Stone (p, _)) =
--     adjacentStones bsize allStones p

-- adjacentStones :: Int -> [Stone] -> Vertex -> [Stone]
-- adjacentStones bsize allStones p =
--     concatMap toStoneList $ adjacentVertices bsize p
--     where
--       toStoneList p' =
--           case toStone allStones p' of
--             Nothing -> []
--             Just stone -> [stone]

-- adjacentFree :: Int -> [Stone] -> Vertex -> [Vertex]
-- adjacentFree bsize allStones p =
--     filter (((==) Nothing) . (toStone allStones)) $ adjacentVertices bsize p


-- freeVertices :: Int -> [Stone] -> [Vertex]
-- freeVertices n ss =
--     (allVertices n) \\ (verticesFromStones ss)



-- verticesFromStones :: [Stone] -> [Vertex]
-- verticesFromStones ss = map (\(Stone (p, _c)) -> p) ss

-- toStone :: [Stone] -> Vertex -> Maybe Stone
-- toStone allStones p =
--     case lookup p allStones' of
--       Nothing -> Nothing
--       Just color -> Just $ Stone (p, color)
--     where
--       allStones' = map (\(Stone s) -> s) allStones


