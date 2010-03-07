{-# OPTIONS -O2 -Wall -Werror -Wwarn #-}

{- |
   Module     : Data.Goban.Utils
   Copyright  : Copyright (C) 2010 Fabian Linzberger
   License    : GNU GPL, version 3 or above

   Maintainer : Fabian Linzberger <e@lefant.net>
   Stability  : experimental
   Portability: probably

Utilities and Types for Goban Implementation

-}

module Data.Goban.Utils ( territory
                        , saneMoves
                        , isSaneMove
                        , stonesColor
                        , isSuicideVertex
                        , isPotentialFullEye
                        , isDead
                        , killedStones
                        , adjacentFree
                        , allVertices
                        , verticesFromStones
                        , groupOfStone
                        , otherColor
                        ) where


import Data.List ((\\), nub)
import Data.Maybe (catMaybes)
-- import Debug.Trace (trace)

import Data.Goban.Goban


saneMoves :: (Goban a) => a -> [Vertex] -> Color -> [Move]
saneMoves goban koBlockeds color =
    map (\v -> StoneMove (Stone (v, color))) $
        filter (isSaneMove goban koBlockeds color) frees
    where
      frees = freeVertices goban
      -- frees =
      --     if length (moveHistory state) > m
      --     then
      --         freeVertices goban
      --     else
      --         freeNonEdgeVertices goban

      -- m = truncate $ sqrt (fromIntegral (sizeOfGoban goban) :: Float)


isSaneMove :: (Goban a) => a -> [Vertex] -> Color -> Vertex -> Bool
isSaneMove goban koBlockeds color p =
          (not (p `elem` koBlockeds )) &&
          (not (isPotentialFullEye goban color p)) &&
          (not (isSuicideVertex goban color p))


territory :: (Goban a) => a -> Color -> Score
territory goban color =
    sum $ map (fromIntegral . length)
            $ filter f (emptyStrings goban)
    where
      f :: [Vertex] -> Bool
      f gs =
          all (((==) color) . stoneColor)
                  $ concatMap (adjacentStones goban) gs

stonesColor :: (Goban a) => a -> Color -> Score
stonesColor goban color =
    fromIntegral $ length $ filter (((==) color) . stoneColor) $ verticesToStones goban $ allVertices (sizeOfGoban goban)


emptyStrings :: (Goban a) => a -> [[Vertex]]
emptyStrings goban =
    emptyStrings' empties []
    where
      empties = (freeVertices goban)

      emptyStrings' [] gs = gs
      emptyStrings' (a : as) gs =
          emptyStrings' (as \\ ma) (ma : gs)
          where
            ma = maxEmptyString a

      maxEmptyString = maxString (adjacentVertices goban) isEmptyVertex

      isEmptyVertex v = (vertexToStone goban v) == Nothing


isSuicideVertex :: (Goban a) => a -> Color -> Vertex -> Bool
isSuicideVertex goban color v =
    isSuicide goban (Stone (v, color))

isPotentialFullEye :: (Goban a) => a -> Color -> Vertex -> Bool
isPotentialFullEye goban color v =
    -- all adjacent vertices must be our Stones
    (length as == length asSC) &&
    -- if there are 4 diagonals
    if length ds >= 4
    then
        -- all but one of them must be ours or empty
        length dsOC <= 1
    else
        -- if there are less diagonals, all must be ours or empty
        length dsOC == 0
    where
      asSC = filter sameColorStone $ verticesToStones goban as
      as = adjacentVertices goban v

      dsOC = filter otherColorStone $ verticesToStones goban ds
      ds = diagonalVertices goban v

      sameColorStone (Stone (_, c)) = color == c
      otherColorStone (Stone (_, c)) = (otherColor color) == c


-- isEyeLike :: (Goban a) => a -> Color -> Vertex -> Bool
-- isEyeLike goban color v =
--     (length vs == length sns)
--     && isSuicide goban (Stone (v, (otherColor color)))
--     where
--       vs = adjacentVertices goban v
--       sns = filter (\(Stone (_p', c')) -> color == c') ns
--       ns = neighbourStones goban (Stone (v, color))


isSuicide :: Goban a => a -> Stone -> Bool
isSuicide goban stone =
    if isDead goban' stone
    then
        -- trace ("isSuicide calling isDead on goban'' for " ++ show stone)
        isDead goban'' stone
    else
        -- trace ("isSuicide returning early, stone alive for " ++ show stone)
        False
    where
      goban'' = deleteStones goban' dead
      dead = killedStones goban' stone
      goban' = addStone goban stone


isDead :: Goban a => a -> Stone -> Bool
isDead goban stone =
    not . null $ deadStones goban stone


-- FIXME:
-- if several killed neighbouring stones are part of the same
-- group it will be found twice here
-- nub at the end works around for scoring
killedStones :: (Goban a) => a -> Stone -> [Stone]
killedStones goban stone@(Stone (_p, color)) =
    nub $ concatMap (deadStones goban) ns
    where
      ns = filter hasOtherColor $ neighbourStones goban stone
      hasOtherColor (Stone (_p', color')) =
          (otherColor color) == color'

deadStones :: Goban a => a -> Stone -> [Stone]
deadStones goban stone@(Stone (_p, color)) =
    anyInMaxStringAlive [stone] []
    where
      anyInMaxStringAlive [] gs =
          gs
      anyInMaxStringAlive (n@(Stone (p, _color)) : ns) gs =
          if null frees
          then
              -- trace ("anyInMaxStringAlive recursing after " ++ show n)
              anyInMaxStringAlive (ns ++ (((fgen n) \\ gs) \\ ns)) (n : gs)
          else
              -- trace ("anyInMaxStringAlive found liberties " ++ show n)
              []
          where
            frees = (adjacentFree goban p)

      genF stone' = neighbourStones goban stone'
      filterF (Stone (_p', color')) =
          color == color'
      fgen n =
          filter filterF $ genF n


-- liberties :: (Goban a) => a -> [Stone] -> Int
-- liberties goban groupStones =
--     length ls
--     where
--       ls = nub ls'
--       ls' =
--           concatMap
--           (adjacentFree goban)
--           (verticesFromStones groupStones)



groupOfStone :: (Goban a) => a -> Stone -> [Stone]
groupOfStone goban stone@(Stone (_p, color)) =
    maxString genF' filterF' stone
    where
      genF' stone' = neighbourStones goban stone'
      filterF' (Stone (_p', color')) =
          color == color'


neighbourStones :: (Goban a) => a -> Stone -> [Stone]
neighbourStones goban (Stone (p, _)) =
    adjacentStones goban p

adjacentStones :: (Goban a) => a -> Vertex -> [Stone]
adjacentStones goban p =
    verticesToStones goban $ adjacentVertices goban p

verticesToStones :: (Goban a) => a -> [Vertex] -> [Stone]
verticesToStones goban ps =
    catMaybes $ fmap (vertexToStone goban) ps



adjacentFree :: (Goban a) => a -> Vertex -> [Vertex]
adjacentFree goban p =
    filter (((==) Nothing) . (vertexToStone goban)) $
           adjacentVertices goban p



verticesFromStones :: [Stone] -> [Vertex]
verticesFromStones ss = map (\(Stone (p, _c)) -> p) ss



-- moveColor :: Move -> Color
-- moveColor (StoneMove stone) = stoneColor stone
-- moveColor (Pass color) = color

stoneColor :: Stone -> Color
stoneColor (Stone (_vertex, color)) = color

otherColor :: Color -> Color
otherColor Black = White
otherColor White = Black








maxString :: (Eq a) => (a -> [a]) -> (a -> Bool) -> a -> [a]
maxString genF filterF p =
    maxString' [p] []
    where
      maxString' [] gs = gs
      maxString' (n : ns) gs =
          maxString' (ns ++ (((fgen n) \\ gs) \\ ns)) (n : gs)
      fgen n =
          filter filterF $ genF n

