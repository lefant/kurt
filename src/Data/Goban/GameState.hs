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
                            , getLeafGameState
                            , updateGameState
                            , scoreGameState
                            , nextMoves
                            , thisMoveColor
                            , nextMoveColor
                            ) where


import Data.List (partition, foldl', (\\))
import Control.Monad.ST (ST)
import qualified Data.IntSet as S


import Data.Goban.Goban
import Data.Goban.Utils
import Data.Goban.STVector (STGoban)
-- import Data.Goban.STVector (saneMoves, territory)
import Data.Goban.STVector (newGoban, size, borderVertices)
import Data.Goban.STVector (addStone, deleteStones)


type VertexSet = S.IntSet

data GameState s = GameState { goban           :: !(STGoban s)
                             , freeVertices    :: !VertexSet
                             , koBlocked       :: !VertexSet
                             , moveHistory     :: ![Move]
                             , komi            :: !Score
                             , blackPrisoners  :: !Score
                             , whitePrisoners  :: !Score
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
                   , koBlocked = S.empty
                   , moveHistory = []
                   , komi = initKomi
                   , blackPrisoners = 0
                   , whitePrisoners = 0
                   }
  where
    initFreeVertices = S.fromList $ [0 .. (size n)] \\ (borderVertices n)





-- compute game state at the end of a move sequence by replaying it
getLeafGameState :: GameState -> [Move] -> GameState
getLeafGameState = foldl' updateGameState





-- STUArray s Coord Char) -> Coord -> Move -> ST s Bool
updateGameState :: GameState -> Move -> GameState
updateGameState state move =
    case move of
      StoneMove stone@(Stone (p, _)) ->
          if p `elem` (koBlocked state)
          then error "updateGameState: move in ko violation"
          else
                  -- trace ("updateGameState: "
                  --        ++ show (
                  --                 (" move ", move)
                  --                 ,(" bp: ", blackPrisoners')
                  --                 ,(" wp: ", whitePrisoners')
                  --                 ,(" dead: ", dead)
                  --                 ,(" dead': ", dead')
                  --                 ,(" bdead': ", bDead)
                  --                 ,(" wdead': ", wDead)
                  --                ))
              state {
                       goban = goban''
                      ,moveHistory = (moveHistory state) ++ [move]
                      ,blackPrisoners = blackPrisoners'
                      ,whitePrisoners = whitePrisoners'
                      ,koBlocked = koBlocked'
              }
          where
            dead = killedStones goban' stone
            goban' = addStone (goban state) stone
            goban'' = deleteStones goban' dead
            -- goban''' = deleteStones goban'' dead'
            -- dead' =
            --     if isDead goban'' stone
            --     then (groupOfStone goban'' stone)
            --     else []
            blackPrisoners' =
                (blackPrisoners state)
                + (fromIntegral $ length bDead)
            whitePrisoners' =
                (whitePrisoners state)
                + (fromIntegral $ length wDead)
            (bDead, wDead) = partition
                             (\(Stone (_, c)) -> c == Black)
                             -- (dead ++ dead')
                             dead

            koBlocked' =
                case dead of
                  [koStone@(Stone (v,_))] ->
                      if [stone] == (killedStones goban' koStone)
                      then [v]
                      else []
                  _ -> []


      Pass _color ->
          state {
                moveHistory = (moveHistory state) ++ [move]
               ,koBlocked = []
              }

      Resign _color ->
          state {
                moveHistory = (moveHistory state) ++ [move]
               ,koBlocked = []
              }


-- updateGameState :: GameState -> Move -> GameState
-- updateGameState state move =
--     case move of
--       StoneMove stone@(Stone (p, _)) ->
--           if p `elem` (koBlocked state)
--           then error "updateGameState: move in ko violation"
--           else
--                   -- trace ("updateGameState: "
--                   --        ++ show (
--                   --                 (" move ", move)
--                   --                 ,(" bp: ", blackPrisoners')
--                   --                 ,(" wp: ", whitePrisoners')
--                   --                 ,(" dead: ", dead)
--                   --                 ,(" dead': ", dead')
--                   --                 ,(" bdead': ", bDead)
--                   --                 ,(" wdead': ", wDead)
--                   --                ))
--               state {
--                        goban = goban''
--                       ,moveHistory = (moveHistory state) ++ [move]
--                       ,blackPrisoners = blackPrisoners'
--                       ,whitePrisoners = whitePrisoners'
--                       ,koBlocked = koBlocked'
--               }
--           where
--             dead = killedStones goban' stone
--             goban' = addStone (goban state) stone
--             goban'' = deleteStones goban' dead
--             -- goban''' = deleteStones goban'' dead'
--             -- dead' =
--             --     if isDead goban'' stone
--             --     then (groupOfStone goban'' stone)
--             --     else []
--             blackPrisoners' =
--                 (blackPrisoners state)
--                 + (fromIntegral $ length bDead)
--             whitePrisoners' =
--                 (whitePrisoners state)
--                 + (fromIntegral $ length wDead)
--             (bDead, wDead) = partition
--                              (\(Stone (_, c)) -> c == Black)
--                              -- (dead ++ dead')
--                              dead

--             koBlocked' =
--                 case dead of
--                   [koStone@(Stone (v,_))] ->
--                       if [stone] == (killedStones goban' koStone)
--                       then [v]
--                       else []
--                   _ -> []


--       Pass _color ->
--           state {
--                 moveHistory = (moveHistory state) ++ [move]
--                ,koBlocked = []
--               }

--       Resign _color ->
--           state {
--                 moveHistory = (moveHistory state) ++ [move]
--                ,koBlocked = []
--               }



scoreGameState :: GameState -> Score
scoreGameState state =
    -- trace ("score: "
    --        ++ show (
    --                 ("s: ", s),
    --                 ("b: ", b),
    --                 ("w: ", w),
    --                 ("bt: ", bt),
    --                 ("bp: ", bp),
    --                 ("wt: ", wt),
    --                 ("wp: ", wp),
    --                 ("k: ", k)
    --                ))
    s
    where
      s = b - w
      b = bt
      w = wt + k
      bt = colorTerritory Black
      wt = colorTerritory White
      k = (komi state)

      colorTerritory color =
          territory (goban state) color
                        + stonesColor (goban state) color





nextMoves :: GameState -> Color -> [Move]
nextMoves gState color =
    saneMoves (goban gState) (koBlocked gState) color



thisMoveColor :: GameState -> Color
thisMoveColor state =
    case moveHistory state of
      [] ->
          error "thisMoveColor called when moveHistory still empty"
      moves ->
          case last moves of
            (StoneMove (Stone (_, color))) -> color
            (Pass color) -> color
            (Resign color) -> color

nextMoveColor :: GameState -> Color
nextMoveColor state =
    case moveHistory state of
      [] -> Black
      moves ->
          otherColor $
          case last moves of
            (StoneMove (Stone (_, color))) -> color
            (Pass color) -> color
            (Resign color) -> color

