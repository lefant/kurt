{-# OPTIONS -O2 -Wall -Werror -Wwarn #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

{- |
   Module     : Data.Goban.GameState
   Copyright  : Copyright (C) 2010 Fabian Linzberger
   License    : GNU GPL, version 3 or above

   Maintainer : Fabian Linzberger <e@lefant.net>
   Stability  : experimental
   Portability: probably

GameState Implementation

-}

module Data.Goban.GameState ( GameState(..)
                            , newGameState
                            , scoreGameState
                            , getLeafGameState
                            , updateGameState
                            , nextMoves
                            , freeVertices
                            -- , freeVertex
                            , thisMoveColor
                            , nextMoveColor
                            ) where


import Control.Monad (filterM, foldM)
import Control.Monad.ST (ST)
import Data.List ((\\))
import qualified Data.IntSet as S


import Data.Goban.Goban
import Data.Goban.Utils
import Data.Goban.STVector
-- import Data.Goban.STVector (newGoban, copyGoban, intToVertex, vertexToInt, maxIntIndex, borderVertices, intAscAdjacentVertices, intVerticesFromStones, intAdjacentStones, isSaneMove, killedStones, showboard)
-- import Data.Goban.STVector (addStone, deleteStones)


import Debug.Trace (trace)


type IntVertexSet = S.IntSet

data GameState s = GameState { goban           :: !(STGoban s)
                             , boardsize       :: !Boardsize
                             , freeVerticesSet :: !IntVertexSet
                             , koBlocked       :: !(Maybe Vertex)
                             , moveHistory     :: ![Move]
                             , komi            :: !Score
                             , blackStones     :: !Int
                             , whiteStones     :: !Int
                             -- , blackPrisoners  :: !Score
                             -- , whitePrisoners  :: !Score
                             }


-- instance Show GameState where
--     show state =
--         case moveHistory state of
--           [] -> ""
--           moves ->
--               show $ last moves
--               -- case last moves of
--               --   (StoneMove (Stone ((x, y), color))) ->
--               --       c ++ [(xToLetter x)] ++ (show y)
--               --       where
--               --         c = case color of
--               --               Black -> "b "
--               --               White -> "w "
--               --   (Pass _color) -> "pass"
--               --   (Resign _color) -> "resign"


newGameState :: Boardsize -> Score -> ST s (GameState s)
newGameState n initKomi = do
  g <- newGoban n
  return GameState { goban = g
                   , boardsize = n
                   , freeVerticesSet = initFreeVertices
                   , koBlocked = Nothing
                   , moveHistory = []
                   , komi = initKomi
                   , blackStones = 0
                   , whiteStones = 0
                   -- , blackPrisoners = 0
                   -- , whitePrisoners = 0
                   }
  where
    initFreeVertices = S.fromList $ [0 .. (maxIntIndex n)] \\ (borderVertices n)




nextMoves :: GameState s -> Color -> ST s [Move]
nextMoves state color =
    filterM (isSaneMove (goban state) color) (freeVertices state) >>=
                (return . (map (\v -> StoneMove (Stone (v, color)))))



-- compute game state at the end of a move sequence by replaying it
getLeafGameState :: GameState s -> [Move] -> ST s (GameState s)
getLeafGameState state moves = do
  g' <- copyGoban $ goban state
  state' <- return $ state { goban = g' }
  foldM updateGameState state' moves



updateGameState :: GameState s -> Move -> ST s (GameState s)
updateGameState state move =
    case move of
      Pass _color ->
          return $ state {
                       moveHistory = (moveHistory state) ++ [move]
                     , koBlocked = Nothing
                     }
      Resign _color ->
          return $ state {
                       moveHistory = (moveHistory state) ++ [move]
                     , koBlocked = Nothing
                     }
      StoneMove stone@(Stone (p, c)) ->
          do
            -- str1 <- showboard g
            -- trace ("updateGameState before" ++ str1) $ return ()
            addStone g stone
            -- str2 <- showboard g
            -- trace ("updateGameState after addStone" ++ str2) $ return ()
            dead <- killedStones g stone
            -- trace ("updateGameState dead" ++ show dead) $ return ()
            deleteStones g dead
            -- str4 <- showboard g
            -- trace ("updateGameState after deleteStones" ++ str4) $ return ()
            return $ state {
                         goban = g
                       , moveHistory = (moveHistory state) ++ [move]
                       , blackStones =
                         (if c == Black
                          then (blackStones state) + 1
                          else (blackStones state) + (length dead))
                       , whiteStones =
                         (if c == White
                          then (whiteStones state) + 1
                          else (whiteStones state) + (length dead))
                       , koBlocked =
                         case dead of
                           [Stone (k, _)] -> Just k
                           _ -> Nothing
                       , freeVerticesSet = freeVerticesSet' p dead
                       }
    where
      g = goban state

      freeVerticesSet' p dead =
          S.union
               (S.fromList $
                 intVerticesFromStones (boardsize state) dead)
               (S.delete
                     (vertexToInt (boardsize state) p)
                     (freeVerticesSet state))


scoreGameState :: GameState s -> ST s Score
scoreGameState state = do
  empties <- return $ emptyStrings state
  colorTs <- mapM (colorTerritories (goban state)) empties
  blackTerritory <- return $ countTerritory Black colorTs
  whiteTerritory <- return $ countTerritory White colorTs
  b <- return $ fromIntegral $ (blackStones state) + blackTerritory
  w <- return $ fromIntegral $ (whiteStones state) + whiteTerritory
  return $ b - w - (komi state)

  where
    countTerritory color ts =
        sum $ map (fromIntegral . length . snd)
                $ filter ((== color) . fst) $ concat ts

colorTerritories :: STGoban s -> [Int] -> ST s ([(Color, [Int])])
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





emptyStrings :: GameState s -> [[Int]]
emptyStrings state =
  emptyStrings' initFrees []

    where
      emptyStrings' frees xs 
          | S.null frees = xs
          | otherwise =
              emptyStrings' frees'' ((S.toList iMax) : xs)
          where
            frees'' = frees' `S.intersection` iMax
            iMax = maxIntSet myAdjacentVertices isFree i
            (i, frees') = S.deleteFindMin frees

      myAdjacentVertices i =
          S.fromDistinctAscList (intAscAdjacentVertices n i)

      -- maybe actually looking it up in the goban is faster?
      isFree i = i `S.member` initFrees

      initFrees = freeVerticesSet state

      n = boardsize state


maxIntSet :: (Int -> S.IntSet) -> (Int -> Bool) -> Int -> S.IntSet
maxIntSet genF filterF p =
    maxIntSet' (S.singleton p) S.empty
    where
      maxIntSet' is js
          | S.null is = js
          | otherwise = maxIntSet' is'' js'
          where
            is'' = S.union is' $ S.difference ks js
            js' = S.insert i js
            ks = S.filter filterF $ genF i
            (i, is') = S.deleteFindMin is






thisMoveColor :: GameState s -> Color
thisMoveColor state =
    case moveHistory state of
      [] ->
          error "thisMoveColor called when moveHistory still empty"
      moves ->
          case last moves of
            (StoneMove (Stone (_, color))) -> color
            (Pass color) -> color
            (Resign color) -> color

nextMoveColor :: GameState s -> Color
nextMoveColor state =
    case moveHistory state of
      [] -> Black
      moves ->
          otherColor $
          case last moves of
            (StoneMove (Stone (_, color))) -> color
            (Pass color) -> color
            (Resign color) -> color


freeVertices :: GameState s -> [Vertex]
freeVertices state =
    map (intToVertex (boardsize state))
            $ S.toList
            $ case koBlocked state of
                Nothing -> freeVerticesSet state
                Just i ->
                    S.delete (vertexToInt (boardsize state) i)
                         $ freeVerticesSet state



-- freeVertex :: GameState s -> Vertex -> Bool
-- freeVertex state p =
--     i `S.member` (freeVerticesSet state)
--     where
--       i = vertexToInt (boardsize state) p
