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

module Data.Goban.STVector ( STGoban(..)
                           , newGoban
                           , addStone
                           , deleteStones
                           , borderVertices
                           , size
                           -- , territory
                           -- , saneMoves
                           -- , isSaneMove
                           -- , stonesColor
                           -- , isSuicideVertex
                           -- , isPotentialFullEye
                           -- , isDead
                           -- , killedStones
                           -- , adjacentFree
                           -- , allVertices
                           -- , verticesFromStones
                           -- , groupOfStone
                           ) where

import Control.Monad (liftM)
import Control.Monad.ST (ST)
import Data.Maybe (catMaybes)
import Data.Word (Word)
import Data.List ((\\), nub)
import qualified Data.Vector.Unboxed.Mutable as VM

-- import Debug.Trace (trace)


import Data.Goban.Goban
import Data.Goban.Utils (verticesFromStones)
-- import Data.Goban.Utils (stoneColor, otherColor, allVertices, inBounds, nonEdgeVertices, maxString)




data STGoban s = STGoban !Boardsize (VM.STVector s Word)

-- this should be some kind of enum instance
-- so we can do toEnum / fromEnum instead of stateToWord
data VertexState = VertexColor Color | Empty | Border


newGoban :: Boardsize -> ST s (STGoban s)
newGoban n = do
  v <- VM.newWith (size n) (stateToWord Empty)
  mapM_ (\i -> VM.write v i (stateToWord Border)) (borderVertices n)
  return $ STGoban n v


addStone :: STGoban s -> Stone -> ST s ()
addStone g (Stone (vertex, color)) =
    writeGoban g vertex (VertexColor color)

deleteStones :: STGoban s -> [Stone] -> ST s ()
deleteStones g stones =
    mapM_ (\p -> writeGoban g p Empty) stoneVertices
    where
      stoneVertices = verticesFromStones stones

vertexToStone :: STGoban s -> Vertex -> ST s (Maybe Stone)
vertexToStone g vertex = do
  s <- readGoban g vertex
  return $ case s of
             VertexColor color -> Just $ Stone (vertex, color)
             Empty -> Nothing


writeGoban :: STGoban s -> Vertex -> VertexState -> ST s ()
writeGoban (STGoban n v) vertex state =
    VM.write v (vertexToInt n vertex) (stateToWord state)

readGoban :: STGoban s -> Vertex -> ST s VertexState
readGoban (STGoban n v) vertex =
    VM.read v (vertexToInt n vertex) >>= (return . wordToState)


gobanSize :: STGoban s -> ST s Boardsize
gobanSize (STGoban n _) = return n





-- helpers: vertex / integer conversion

borderVertices :: Boardsize -> [Int]
borderVertices n =
    map (vertexToInt n)
            [(x, y) | x <- [0, maxEdge n], y <- [0, maxEdge n] ]

{-# INLINE vertexToInt #-}
vertexToInt :: Boardsize -> Vertex -> Int
vertexToInt n (x, y)
    | x > n = error "vertexToInt: x > boardsize"
    | x < 0 = error "vertexToInt: x < 1"
    | y > n = error "vertexToInt: y > boardsize"
    | y < 0 = error "vertexToInt: y < 1"
vertexToInt n (x, y) =
    y * n + x

{-# INLINE intToVertex #-}
intToVertex :: Boardsize -> Int -> Vertex
intToVertex n i
    | i < 0 = error "intToVertex: n < 0"
    | i > ((n + 2) ^ (2 :: Int)) =
        error "intToVertex: n > ((boardsize+2) ^ 2)"
intToVertex n i =
    (x, y)
    where
      y = (i `div` maxEdge2)
      x = (i `mod` maxEdge2)
      -- double check this does not need to be (maxEdge n)
      maxEdge2 = n + 2

{-# INLINE size #-}
size :: Boardsize -> Int
size n = 1 + (vertexToInt n (maxEdge n, maxEdge n))

{-# INLINE maxEdge #-}
maxEdge :: Boardsize -> Int
maxEdge n = n + 1



-- helpers: state / word conversion

stateToWord :: VertexState -> Word
stateToWord Empty = 0
stateToWord (VertexColor Black) = 1
stateToWord (VertexColor White) = 2
stateToWord Border = 3

{-# INLINE stateToWord #-}

wordToState :: Word -> VertexState
wordToState n =
    case (fromIntegral n) :: Int of
      1 -> VertexColor Black
      2 -> VertexColor White
      3 -> Border
      other -> error ("wordToState parameter out of range " ++ show other)

{-# INLINE wordToState #-}

-- wordToState 0 = Empty
-- wordToState 1 = VertexColor Black
-- wordToState 2 = VertexColor White
-- wordToState 3 = Border
-- wordToState _ = error "intToState parameter out of range"





















-- adjacentVertices :: STGoban -> Vertex -> [Vertex]
-- adjacentVertices goban (x, y) =
--     filter
--     (inBounds (sizeOfGoban goban))
--     [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]

-- diagonalVertices :: STGoban -> Vertex -> [Vertex]
-- diagonalVertices goban (x, y) =
--     filter
--     (inBounds (sizeOfGoban goban))
--     [(x+1,y+1),(x-1,y+1),(x+1,y-1),(x-1,y-1)]

-- freeNonEdgeVertices :: STGoban -> [Vertex]
-- freeNonEdgeVertices goban =
--     filter (((==) Nothing) . (vertexToStone goban)) $
--            nonEdgeVertices (sizeOfGoban goban)

-- showboard :: STGoban -> String
-- showboard goban =
--     show' $ map (vertexToStone goban) $ allVertices (sizeOfGoban goban)
--     where
--       show' [] = ""
--       show' ls' = concatMap showStone left ++ "\n" ++ show' right
--           where
--             (left, right) = splitAt n ls'
--       n = sizeOfGoban goban
--       showStone Nothing = "."
--       showStone (Just (Stone (_, color)))
--           | color == Black = "x"
--           | color == White = "o"
--       showStone something = error ("showStone: unmatched " ++ show something)












-- saneMoves :: STGoban -> [Vertex] -> Color -> [Move]
-- saneMoves goban koBlockeds color =
--     map (\v -> StoneMove (Stone (v, color))) $
--         filter (isSaneMove goban koBlockeds color) frees
--     where
--       frees = freeVertices goban
--       -- frees =
--       --     if length (moveHistory state) > m
--       --     then
--       --         freeVertices goban
--       --     else
--       --         freeNonEdgeVertices goban

--       -- m = truncate $ sqrt (fromIntegral (sizeOfGoban goban) :: Float)


-- isSaneMove :: STGoban -> [Vertex] -> Color -> Vertex -> Bool
-- isSaneMove goban koBlockeds color p =
--           (not (p `elem` koBlockeds )) &&
--           (not (isPotentialFullEye goban color p)) &&
--           (not (isSuicideVertex goban color p))


-- territory :: STGoban -> Color -> Score
-- territory goban color =
--     sum $ map (fromIntegral . length)
--             $ filter f (emptyStrings goban)
--     where
--       f :: [Vertex] -> Bool
--       f gs =
--           all (((==) color) . stoneColor)
--                   $ concatMap (adjacentStones goban) gs

-- stonesColor :: STGoban -> Color -> Score
-- stonesColor goban color =
--     fromIntegral $ length $ filter (((==) color) . stoneColor) $ verticesToStones goban $ allVertices (sizeOfGoban goban)


-- emptyStrings :: STGoban -> [[Vertex]]
-- emptyStrings goban =
--     emptyStrings' empties []
--     where
--       empties = (freeVertices goban)

--       emptyStrings' [] gs = gs
--       emptyStrings' (a : as) gs =
--           emptyStrings' (as \\ ma) (ma : gs)
--           where
--             ma = maxEmptyString a

--       maxEmptyString = maxString (adjacentVertices goban) isEmptyVertex

--       isEmptyVertex v = (vertexToStone goban v) == Nothing


-- isSuicideVertex :: STGoban -> Color -> Vertex -> Bool
-- isSuicideVertex goban color v =
--     isSuicide goban (Stone (v, color))

-- isPotentialFullEye :: STGoban -> Color -> Vertex -> Bool
-- isPotentialFullEye goban color v =
--     -- all adjacent vertices must be our Stones
--     (length as == length asSC) &&
--     -- if there are 4 diagonals
--     if length ds >= 4
--     then
--         -- all but one of them must be ours or empty
--         length dsOC <= 1
--     else
--         -- if there are less diagonals, all must be ours or empty
--         length dsOC == 0
--     where
--       asSC = filter sameColorStone $ verticesToStones goban as
--       as = adjacentVertices goban v

--       dsOC = filter otherColorStone $ verticesToStones goban ds
--       ds = diagonalVertices goban v

--       sameColorStone (Stone (_, c)) = color == c
--       otherColorStone (Stone (_, c)) = (otherColor color) == c


-- -- isEyeLike :: STGoban -> Color -> Vertex -> Bool
-- -- isEyeLike goban color v =
-- --     (length vs == length sns)
-- --     && isSuicide goban (Stone (v, (otherColor color)))
-- --     where
-- --       vs = adjacentVertices goban v
-- --       sns = filter (\(Stone (_p', c')) -> color == c') ns
-- --       ns = neighbourStones goban (Stone (v, color))


-- isSuicide :: STGoban -> Stone -> Bool
-- isSuicide goban stone =
--     if isDead goban' stone
--     then
--         -- trace ("isSuicide calling isDead on goban'' for " ++ show stone)
--         isDead goban'' stone
--     else
--         -- trace ("isSuicide returning early, stone alive for " ++ show stone)
--         False
--     where
--       goban'' = deleteStones goban' dead
--       dead = killedStones goban' stone
--       goban' = addStone goban stone


-- isDead :: STGoban -> Stone -> Bool
-- isDead goban stone =
--     not . null $ deadStones goban stone


-- -- FIXME:
-- -- if several killed neighbouring stones are part of the same
-- -- group it will be found twice here
-- -- nub at the end works around for scoring
-- killedStones :: STGoban -> Stone -> [Stone]
-- killedStones goban stone@(Stone (_p, color)) =
--     nub $ concatMap (deadStones goban) ns
--     where
--       ns = filter hasOtherColor $ neighbourStones goban stone
--       hasOtherColor (Stone (_p', color')) =
--           (otherColor color) == color'

-- deadStones :: STGoban -> Stone -> [Stone]
-- deadStones goban stone@(Stone (_p, color)) =
--     anyInMaxStringAlive [stone] []
--     where
--       anyInMaxStringAlive [] gs =
--           gs
--       anyInMaxStringAlive (n@(Stone (p, _color)) : ns) gs =
--           if null frees
--           then
--               -- trace ("anyInMaxStringAlive recursing after " ++ show n)
--               anyInMaxStringAlive (ns ++ (((fgen n) \\ gs) \\ ns)) (n : gs)
--           else
--               -- trace ("anyInMaxStringAlive found liberties " ++ show n)
--               []
--           where
--             frees = (adjacentFree goban p)

--       genF stone' = neighbourStones goban stone'
--       filterF (Stone (_p', color')) =
--           color == color'
--       fgen n =
--           filter filterF $ genF n


-- -- liberties :: STGoban -> [Stone] -> Int
-- -- liberties goban groupStones =
-- --     length ls
-- --     where
-- --       ls = nub ls'
-- --       ls' =
-- --           concatMap
-- --           (adjacentFree goban)
-- --           (verticesFromStones groupStones)



-- groupOfStone :: STGoban -> Stone -> [Stone]
-- groupOfStone goban stone@(Stone (_p, color)) =
--     maxString genF' filterF' stone
--     where
--       genF' stone' = neighbourStones goban stone'
--       filterF' (Stone (_p', color')) =
--           color == color'


-- neighbourStones :: STGoban -> Stone -> [Stone]
-- neighbourStones goban (Stone (p, _)) =
--     adjacentStones goban p

-- adjacentStones :: STGoban -> Vertex -> [Stone]
-- adjacentStones goban p =
--     verticesToStones goban $ adjacentVertices goban p

-- verticesToStones :: STGoban -> [Vertex] -> [Stone]
-- verticesToStones goban ps =
--     catMaybes $ fmap (vertexToStone goban) ps



-- adjacentFree :: STGoban -> Vertex -> [Vertex]
-- adjacentFree goban p =
--     filter (((==) Nothing) . (vertexToStone goban)) $
--            adjacentVertices goban p





