{-# OPTIONS -O2 -Wall -Werror -Wwarn #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

{- |
   Module     : Data.Goban.GameState
   Copyright  : Copyright (C) 2010 Fabian Linzberger
   License    : GNU GPL, version 3 or above

   Maintainer : Fabian Linzberger <e@lefant.net>
   Stability  : experimental
   Portability: probably

Go GameState Implementation

-}

module Data.Goban.GameState ( GameState(..)
                            , newGameState
                            , showGameState
                            , scoreGameState
                            , getLeafGameState
                            , updateGameState
                            , centerHeuristic
                            , nextMoves
                            , freeVertices
                            , thisMoveColor
                            , nextMoveColor
                            ) where


import Control.Monad (filterM, foldM)
import Control.Monad.ST (ST)
import Data.List ((\\))
import qualified Data.IntSet as S


import Data.Goban.Types
import Data.Goban.IntVertex
import Data.Tree.UCT (UCTHeuristic)
import Data.Goban.Utils
import Data.Goban.STVectorGoban
import Data.Goban.Incremental

-- import Debug.TraceOrId (trace)



type IntVertexSet = S.IntSet

data GameState s = GameState { goban           :: !(STGoban s)
                             , chainGoban      :: !(ChainIdGoban s)
                             , chains          :: !ChainMap
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

showGameState :: GameState s -> ST s String
showGameState state = do
  gobanStr <- showGoban $ goban state
  chainGobanStr <- showChainIdGoban $ chainGoban state
  return $ (unlines $ zipWith (++) (lines gobanStr) $ [
                       ""
                      ,""
                      ,""
                      ,"   blackStones: " ++ show (blackStones state)
                      ,"   whiteStones: " ++ show (whiteStones state)
                      ,"   komi: " ++ show (komi state)
                      ,""
                      ,"   koBlocked: " ++ show (koBlocked state)
                      ] ++ repeat ""
          -- ++ " moveHistory: " ++ show (moveHistory state)
          -- ++ "\n"
          -- ++ "freeVerticesSet: " ++ show (freeVerticesSet state)
          -- ++ "\n"
         ) ++ "\n" ++ chainGobanStr


newGameState :: Boardsize -> Score -> ST s (GameState s)
newGameState n initKomi = do
  g <- newGoban n
  cg <- newChainGoban n
  return GameState { goban = g
                   , chainGoban = cg
                   , chains = newChainMap
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
nextMoves state color = do
  freeStones <- return $ map (((flip Stone) color)) $ freeVertices state
  sanes <- filterM (isSaneMove (goban state)) freeStones
  return $ map Move sanes



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
      Move stone@(Stone p c) ->
          do
            -- incremental
            chains' <- addChainStone (chainGoban state) (chains state) stone

            -- str1 <- showGoban g
            -- trace ("updateGameState before" ++ str1) $ return ()
            addStone g stone
            -- str2 <- showGoban g
            -- trace ("updateGameState after addStone" ++ str2) $ return ()
            dead <- killedStones g stone
            -- trace ("updateGameState dead" ++ show dead) $ return ()
            deleteStones g dead
            -- str4 <- showGoban g
            -- trace ("updateGameState after deleteStones" ++ str4) $ return ()
            return $ state {
                         goban = g
                       , chains = chains'
                       , moveHistory = (moveHistory state) ++ [move]
                       , blackStones =
                         (if c == Black
                          then (blackStones state) + 1
                          else (blackStones state) - (length dead))
                       , whiteStones =
                         (if c == White
                          then (whiteStones state) + 1
                          else (whiteStones state) - (length dead))
                       , koBlocked =
                         case dead of
                           [Stone k _] -> Just k
                           _ -> Nothing
                       , freeVerticesSet = freeVerticesSet' p dead
                       }
    where
      g = goban state

      freeVerticesSet' p dead =
          S.union
               (S.fromList $
                 map ((vertexToInt (boardsize state)) . stoneVertex) dead)
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
  -- trace ("scoreGameState empties " ++ show empties) $ return ()
  -- trace ("scoreGameState colorTs " ++ show colorTs) $ return ()
  -- trace ("scoreGameState stones " ++ show (blackStones state, whiteStones state)) $ return ()
  -- trace ("scoreGameState " ++ show ((blackTerritory, b), (whiteTerritory, w))) $ return ()
  return $ b - w - (komi state)

  where
    countTerritory color ts =
        sum $ map (fromIntegral . length . snd)
                $ filter ((== color) . fst) $ concat ts


emptyStrings :: GameState s -> [[Int]]
emptyStrings state =
  emptyStrings' initFrees []

    where
      emptyStrings' frees xs 
          | S.null frees = xs
          | otherwise =
              emptyStrings' frees'' ((S.toList iMax) : xs)
          where
            frees'' = frees' `S.difference` iMax
            iMax = maxIntSet myAdjacentVertices isFree i
            (i, frees') = S.deleteFindMin frees

      myAdjacentVertices i =
          S.fromDistinctAscList (intAscAdjacentVertices n i)

      -- maybe actually looking it up in the goban is faster?
      isFree i = i `S.member` initFrees

      initFrees = freeVerticesSet state

      n = boardsize state





centerHeuristic :: GameState s -> UCTHeuristic Move
centerHeuristic state (Move (Stone (x, y) _color)) =
    -- trace ("centerHeuristic " ++ show (s, (h, (l, m, beta, halfBeta), result)))
    result
    where
      result = (0.5 - halfBeta + beta * h, 7)

      -- must be between 0 and 1
      h = fromIntegral (minimum [ x, n - x + 1, y, n - y + 1, 3]) ^ (2 :: Int)
          / 9

      -- scaling factor between 1 at the beginning and 0 when l gets big
      beta = fromIntegral m / fromIntegral (l + m)
      halfBeta = beta / 2

      l = length $ moveHistory state
      n = boardsize state
      m = n
centerHeuristic _ _ = error "centerHeuristic received non StoneMove arg"







thisMoveColor :: GameState s -> Color
thisMoveColor state =
    case moveHistory state of
      [] ->
          error "thisMoveColor called when moveHistory still empty"
      moves ->
          case last moves of
            Move (Stone _ color) -> color
            Pass color -> color
            Resign color -> color

nextMoveColor :: GameState s -> Color
nextMoveColor state =
    case moveHistory state of
      [] -> Black
      moves ->
          otherColor $
          case last moves of
            Move (Stone _ color) -> color
            Pass color -> color
            Resign color -> color


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
