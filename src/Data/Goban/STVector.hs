{-# OPTIONS -O2 -Wall -Werror -Wwarn #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

{- |
   Module     : Data.Goban.Vector
   Copyright  : Copyright (C) 2010 Fabian Linzberger
   License    : GNU GPL, version 3 or above

   Maintainer : Fabian Linzberger <e@lefant.net>
   Stability  : experimental
   Portability: probably

Goban Implementation using Data.Vector

-}

module Data.Goban.STVector ( STVectorGoban(..)
                           , territory
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
                           , scoreToResult
                           , winningScore
                           ) where

import qualified Data.Vector.Unboxed.Mutable as V
-- import Debug.Trace (trace)


import Data.Goban.Goban


newtype STVectorGoban = VectorGoban (Int, (V.STVector Int))
    deriving (Show)

data VertexState = VertexColor Color | Empty
                 deriving (Show, Eq)

instance STGoban STVectorGoban where

    addStone (VectorGoban (boardsize, goban)) (Stone (vertex, color)) =
        -- trace ("addStone: " ++ show (intVertex, intState))
        VectorGoban (boardsize, goban')
        where
          goban' = goban V.// [(intVertex, intState)]
          intVertex = vertexToInt boardsize vertex
          intState = stateToInt (VertexColor color)


    deleteStones (VectorGoban (boardsize, goban)) stones =
        VectorGoban (boardsize, goban')
        where
          goban' = goban V.// (map intPair stones)
          intPair (Stone (vertex, _)) =
              (intVertex, intState)
              where
                intVertex = vertexToInt boardsize vertex
                intState = stateToInt Empty

    freeVertices (VectorGoban (boardsize, goban)) =
        map (intToVertex boardsize) $ V.toList $
            V.findIndices ((stateToInt Empty) ==) goban

    vertexToStone (VectorGoban (boardsize, goban)) p =
        -- trace ("vertexToStone " ++ show (p, intVertex))
        result
        where
          result =
              case intToState $ goban V.! intVertex of
                VertexColor color -> Just $ Stone (p, color)
                Empty -> Nothing
          intVertex = vertexToInt boardsize p

    sizeOfGoban (VectorGoban (boardsize, _)) = boardsize

    newGoban boardsize =
        VectorGoban
        (boardsize,
         V.replicate
              (1 + (vertexToInt boardsize (boardsize, boardsize)))
              (stateToInt Empty))



-- helpers: vertex / integer conversion

vertexToInt :: Int -> Vertex -> Int
vertexToInt boardsize (x, y)
    | x > boardsize = error "vertexToInt: x > boardsize"
    | x < 1 = error "vertexToInt: x < 1"
    | y > boardsize = error "vertexToInt: y > boardsize"
    | y < 1 = error "vertexToInt: y < 1"
vertexToInt boardsize (x, y) =
    y' * boardsize + x'
    where
      x' = x - 1
      y' = y - 1

{-# INLINE vertexToInt #-}

intToVertex :: Int -> Int -> Vertex
intToVertex boardsize n
    | n < 0 = error "intToVertex: n < 0"
    | n > (boardsize ^ (2 :: Int)) = error "intToVertex: n > (boardsize ^ 2)"
intToVertex boardsize n =
    (x, y)
    where
      y = (n `div` boardsize) + 1
      x = (n `mod` boardsize) + 1


{-# INLINE intToVertex #-}


stateToInt :: VertexState -> Int
stateToInt Empty = 0
stateToInt (VertexColor Black) = 1
stateToInt (VertexColor White) = 2

intToState :: Int -> VertexState
intToState 0 = Empty
intToState 1 = VertexColor Black
intToState 2 = VertexColor White
intToState _ = error "intToState parameter out of range"





















adjacentVertices :: STVectorGoban -> Vertex -> [Vertex]
adjacentVertices goban (x, y) =
    filter
    (inBounds (sizeOfGoban goban))
    [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]

diagonalVertices :: STVectorGoban -> Vertex -> [Vertex]
diagonalVertices goban (x, y) =
    filter
    (inBounds (sizeOfGoban goban))
    [(x+1,y+1),(x-1,y+1),(x+1,y-1),(x-1,y-1)]

freeNonEdgeVertices :: STVectorGoban -> [Vertex]
freeNonEdgeVertices goban =
    filter (((==) Nothing) . (vertexToStone goban)) $
           nonEdgeVertices (sizeOfGoban goban)

showboard :: STVectorGoban -> String
showboard goban =
    show' $ map (vertexToStone goban) $ allVertices (sizeOfGoban goban)
    where
      show' [] = ""
      show' ls' = concatMap showStone left ++ "\n" ++ show' right
          where
            (left, right) = splitAt n ls'
      n = sizeOfGoban goban
      showStone Nothing = "."
      showStone (Just (Stone (_, color)))
          | color == Black = "x"
          | color == White = "o"
      showStone something = error ("showStone: unmatched " ++ show something)












saneMoves :: STVectorGoban -> [Vertex] -> Color -> [Move]
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


isSaneMove :: STVectorGoban -> [Vertex] -> Color -> Vertex -> Bool
isSaneMove goban koBlockeds color p =
          (not (p `elem` koBlockeds )) &&
          (not (isPotentialFullEye goban color p)) &&
          (not (isSuicideVertex goban color p))


territory :: STVectorGoban -> Color -> Score
territory goban color =
    sum $ map (fromIntegral . length)
            $ filter f (emptyStrings goban)
    where
      f :: [Vertex] -> Bool
      f gs =
          all (((==) color) . stoneColor)
                  $ concatMap (adjacentStones goban) gs

stonesColor :: STVectorGoban -> Color -> Score
stonesColor goban color =
    fromIntegral $ length $ filter (((==) color) . stoneColor) $ verticesToStones goban $ allVertices (sizeOfGoban goban)


emptyStrings :: STVectorGoban -> [[Vertex]]
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


isSuicideVertex :: STVectorGoban -> Color -> Vertex -> Bool
isSuicideVertex goban color v =
    isSuicide goban (Stone (v, color))

isPotentialFullEye :: STVectorGoban -> Color -> Vertex -> Bool
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


-- isEyeLike :: STVectorGoban -> Color -> Vertex -> Bool
-- isEyeLike goban color v =
--     (length vs == length sns)
--     && isSuicide goban (Stone (v, (otherColor color)))
--     where
--       vs = adjacentVertices goban v
--       sns = filter (\(Stone (_p', c')) -> color == c') ns
--       ns = neighbourStones goban (Stone (v, color))


isSuicide :: STVectorGoban -> Stone -> Bool
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


isDead :: STVectorGoban -> Stone -> Bool
isDead goban stone =
    not . null $ deadStones goban stone


-- FIXME:
-- if several killed neighbouring stones are part of the same
-- group it will be found twice here
-- nub at the end works around for scoring
killedStones :: STVectorGoban -> Stone -> [Stone]
killedStones goban stone@(Stone (_p, color)) =
    nub $ concatMap (deadStones goban) ns
    where
      ns = filter hasOtherColor $ neighbourStones goban stone
      hasOtherColor (Stone (_p', color')) =
          (otherColor color) == color'

deadStones :: STVectorGoban -> Stone -> [Stone]
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


-- liberties :: STVectorGoban -> [Stone] -> Int
-- liberties goban groupStones =
--     length ls
--     where
--       ls = nub ls'
--       ls' =
--           concatMap
--           (adjacentFree goban)
--           (verticesFromStones groupStones)



groupOfStone :: STVectorGoban -> Stone -> [Stone]
groupOfStone goban stone@(Stone (_p, color)) =
    maxString genF' filterF' stone
    where
      genF' stone' = neighbourStones goban stone'
      filterF' (Stone (_p', color')) =
          color == color'


neighbourStones :: STVectorGoban -> Stone -> [Stone]
neighbourStones goban (Stone (p, _)) =
    adjacentStones goban p

adjacentStones :: STVectorGoban -> Vertex -> [Stone]
adjacentStones goban p =
    verticesToStones goban $ adjacentVertices goban p

verticesToStones :: STVectorGoban -> [Vertex] -> [Stone]
verticesToStones goban ps =
    catMaybes $ fmap (vertexToStone goban) ps



adjacentFree :: STVectorGoban -> Vertex -> [Vertex]
adjacentFree goban p =
    filter (((==) Nothing) . (vertexToStone goban)) $
           adjacentVertices goban p





