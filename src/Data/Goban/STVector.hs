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
                           , gobanSize
                           , intToVertex
                           , vertexToInt
                           , borderVertices
                           -- , maxEdge
                           , maxIntIndex
                           , isSaneMove
                           , isSuicideVertex
                           , isPotentialFullEye
                           , killedStones
                           , neighbourStones
                           , adjacentStones
                           , intAdjacentStones
                           , verticesToStones
                           , adjacentVertices
                           , intAscAdjacentVertices
                           , showboard
                           ) where

import Control.Monad (liftM, filterM)
import Control.Monad.ST (ST)
import Data.Maybe (catMaybes)
import Data.Word (Word)
import Data.List ((\\), nub)
import qualified Data.Vector.Unboxed.Mutable as VM

-- import Debug.Trace (trace)


import Data.Goban.Goban
import Data.Goban.Utils (verticesFromStones, otherColor)
-- import Data.Goban.Utils (stoneColor, otherColor, allVertices, inBounds, nonEdgeVertices, maxString)




data STGoban s = STGoban !Boardsize (VM.STVector s Word)

-- this should be some kind of enum instance
-- so we can do toEnum / fromEnum instead of stateToWord
data VertexState = VertexColor Color | Empty | Border
                 deriving (Eq)


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
             Border -> Nothing

intVertexToStone :: STGoban s -> Int -> ST s (Maybe Stone)
intVertexToStone g@(STGoban n _v) i = do
  s <- intReadGoban g i
  return $ case s of
             VertexColor color -> Just $ Stone (intToVertex n i, color)
             Empty -> Nothing
             Border -> Nothing


writeGoban :: STGoban s -> Vertex -> VertexState -> ST s ()
writeGoban (STGoban n v) vertex state =
    VM.write v (vertexToInt n vertex) (stateToWord state)

readGoban :: STGoban s -> Vertex -> ST s VertexState
readGoban (STGoban n v) vertex =
    VM.read v (vertexToInt n vertex) >>= (return . wordToState)

intReadGoban :: STGoban s -> Int -> ST s VertexState
intReadGoban (STGoban _n v) i =
    VM.read v i >>= (return . wordToState)


gobanSize :: STGoban s -> ST s Boardsize
gobanSize (STGoban n _) = return n





-- helpers: vertex / integer conversion

borderVertices :: Boardsize -> [Int]
borderVertices n =
    map (vertexToInt n) $
            [(x, y) | x <- [0, n + 1], y <- [0 .. n + 1] ]
            ++ [(x, y) | x <- [1 .. n], y <- [0, n + 1] ]

{-# INLINE vertexToInt #-}
vertexToInt :: Boardsize -> Vertex -> Int
vertexToInt n (x, y)
    | x > n + 1 = error "vertexToInt: x > n + 1"
    | x < 0 = error "vertexToInt: x < 0"
    | y > n + 1 = error "vertexToInt: y > n + 1"
    | y < 0 = error "vertexToInt: y < 0"
vertexToInt n (x, y) =
    y * (n + 2) + x

{-# INLINE intToVertex #-}
intToVertex :: Boardsize -> Int -> Vertex
intToVertex n i
    | i < 0 = error "intToVertex: n < 0"
    | i > size n =
        error "intToVertex: i > size n"
intToVertex n i =
    (x, y)
    where
      y = i `div` (n + 2)
      x = i `mod` (n + 2)

{-# INLINE size #-}
size :: Boardsize -> Int
size n = maxIntIndex n + 1

{-# INLINE maxIntIndex #-}
maxIntIndex :: Boardsize -> Int
maxIntIndex n = vertexToInt n (n + 1, n + 1)

-- {-# INLINE maxEdge #-}
-- maxEdge :: Boardsize -> Int
-- maxEdge n = n + 1



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
      0 -> Empty
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





isSaneMove :: STGoban s -> Color -> Vertex -> ST s Bool
isSaneMove g color p =
    -- (not (isPotentialFullEye g color p)) &&
    isSuicideVertex g color p >>= (return . not)



isPotentialFullEye :: STGoban s -> Color -> Vertex -> ST s Bool
isPotentialFullEye g color v = do
  as <- mapM (readGoban g) $ adjacentVertices v
  (if all isSameColorOrBorder as
   then do
     ds <- mapM (readGoban g) $ diagonalVertices v
     opponentDiagonalCount <- return $ length $ filter isOtherColor ds
     return $ ((opponentDiagonalCount == 0)
               || ((length ds == 4) && opponentDiagonalCount == 1))
   else return False)

   where
     isSameColorOrBorder c = c == Border || c == VertexColor color
     isOtherColor c = c == VertexColor (otherColor color)



-- isEyeLike :: STGoban -> Color -> Vertex -> Bool
-- isEyeLike goban color v =
--     (length vs == length sns)
--     && isSuicide goban (Stone (v, (otherColor color)))
--     where
--       vs = adjacentVertices goban v
--       sns = filter (\(Stone (_p', c')) -> color == c') ns
--       ns = neighbourStones goban (Stone (v, color))


isSuicideVertex :: STGoban s -> Color -> Vertex -> ST s Bool
isSuicideVertex g color v =
    isSuicide g (Stone (v, color))


isSuicide :: STGoban s -> Stone -> ST s Bool
isSuicide g stone = do
  addStone g stone
  a <- isDead g stone
  (if a
   then do
     dead <- killedStones g stone
     deleteStones g dead
     isDead g stone
   else return False)


isDead :: STGoban s -> Stone -> ST s Bool
isDead g stone =
    deadStones g stone >>= (return . not . null)

deadStones :: STGoban s -> Stone -> ST s [Stone]
deadStones g stone@(Stone (_p, color)) =
    anyInMaxStringAlive [stone] []
    where
      anyInMaxStringAlive [] gs = return gs
      anyInMaxStringAlive (n@(Stone (p, _color)) : ns) gs = do
          frees <- adjacentFree g p
          (if null frees
           then do
               hs <- liftM (filter filterF) $ genF n
               anyInMaxStringAlive (ns ++ (((hs) \\ gs) \\ ns)) (n : gs)
           else return [])

      genF = neighbourStones g
      filterF (Stone (_p', color')) =
          color == color'




-- FIXME:
-- if several killed neighbouring stones are part of the same
-- group it will be found twice here
-- nub at the end works around for scoring
killedStones :: STGoban s -> Stone -> ST s [Stone]
killedStones g stone@(Stone (_p, color)) =
    neighbourStones g stone >>=
    filterM (return . hasOtherColor) >>=
    mapM (deadStones g) >>= (return . nub . concat)
    where
      hasOtherColor (Stone (_p', color')) =
          (otherColor color) == color'






neighbourStones :: STGoban s -> Stone -> ST s [Stone]
neighbourStones g (Stone (p, _)) =
    adjacentStones g p


adjacentStones :: STGoban s -> Vertex -> ST s [Stone]
adjacentStones g p =
    verticesToStones g $ adjacentVertices p

intAdjacentStones :: STGoban s -> Int -> ST s [Stone]
intAdjacentStones g@(STGoban n _v) p =
    intVerticesToStones g $ intAscAdjacentVertices n p


verticesToStones :: STGoban s -> [Vertex] -> ST s [Stone]
verticesToStones g ps =
    mapM (vertexToStone g) ps >>= (return . catMaybes)

intVerticesToStones :: STGoban s -> [Int] -> ST s [Stone]
intVerticesToStones g ps =
    mapM (intVertexToStone g) ps >>= (return . catMaybes)
  

adjacentFree :: STGoban s -> Vertex -> ST s [Vertex]
adjacentFree g initP = do
  filterM isFree $ adjacentVertices initP
  where
    isFree p = readGoban g p >>= (return . ((==) Empty))



adjacentVertices :: Vertex -> [Vertex]
adjacentVertices (x, y) =
    [(x,y-1),(x-1,y),(x+1,y),(x,y+1)]

intAscAdjacentVertices :: Boardsize -> Int -> [Int]
intAscAdjacentVertices n vertex =
    -- make sure to be in ascending order
    map (vertexToInt n) [(x,y-1),(x-1,y),(x+1,y),(x,y+1)]
    where
      (x, y) = (intToVertex n) vertex

diagonalVertices :: Vertex -> [Vertex]
diagonalVertices (x, y) =
    [(x+1,y+1),(x-1,y+1),(x+1,y-1),(x-1,y-1)]



showboard :: STGoban s -> ST s String
showboard g =
    return "showboard currently unimplemented"

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
--           (verticesFromStones groupStones)



-- freeNonEdgeVertices :: STGoban -> [Vertex]
-- freeNonEdgeVertices goban =
--     filter (((==) Nothing) . (vertexToStone goban)) $
--            nonEdgeVertices (sizeOfGoban goban)

