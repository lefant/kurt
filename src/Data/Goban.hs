{-# OPTIONS -O2 -Wall -Werror -Wwarn #-}

{- |
   Module     : Data.Goban
   Copyright  : Copyright (C) 2010 Fabian Linzberger
   License    : GNU GPL, version 3 or above

   Maintainer : Fabian Linzberger <e@lefant.net>
   Stability  : experimental
   Portability: probably

Goban / GameState Implementation

-}

module Data.Goban ( updateGameState
                  , score
                  , winningScore
                  , saneMoves
                  , thisMoveColor
                  ) where

import Data.List (partition)
import System.Random (RandomGen)
import Control.Monad.Random (Rand, getRandomR)
import Data.List ((\\))
-- import Debug.Trace (trace)

import Data.Goban.GameState
import Data.Goban.Utils
import Data.Goban.Goban
-- import Data.Goban.DataMap (DataMapGoban)
-- import Data.Goban.Array (ArrayGoban)
import Data.Tree.UCT.GameTree (UctNode(..))




instance UctNode GameState where
    isTerminalNode state =
        case reverse $ moveHistory state of
          [] -> False
          [_] -> False
          ((Pass _) : (Pass _) : _) -> True
          _ -> False

    finalResult state =
        -- trace ("finalResult: " ++
        --        (show ((take 5 $ reverse (moveHistory state)), thisScore, res, color)))
        res
        where
          res = scoreToResult color thisScore
          color = thisMoveColor state
          thisScore = score state
    -- randomEvalOnce _state =
    --     getRandomR (0, 1)
    randomEvalOnce state = do
        s <- runOneRandom state
        return $ scoreToResult (thisMoveColor state) s

    children state =
        case saneMoves state of
          [] -> [updateGameState state (Pass color)]
          vs -> map (\v ->
                         updateGameState state (StoneMove (Stone (v, color)))) vs
        where
          color = nextMoveColor state






scoreToResult :: Color -> Score -> Float
scoreToResult color thisScore =
    if thisScore == 0
    then 0.5
    else
        if winningScore color thisScore
        then 0.9 + bonus
        else 0.1 - bonus
    where
      bonus =
          ((sqrt . (max 99) . abs) thisScore) / 100

winningScore :: Color -> Score -> Bool
winningScore color thisScore =
    case color of
      Black -> thisScore > 0
      White -> thisScore < 0

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


score :: GameState -> Score
score state =
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



runOneRandom :: (RandomGen g) => GameState -> Rand g Score
runOneRandom initState =
    run initState 0
    where
      run :: (RandomGen g) => GameState -> Int -> Rand g Score
      run _ 1000 = return 0
      run state runCount = do
        move <- genMoveRand state
        state' <- return $ updateGameState state move
        case move of
          (Pass _) -> do
                    move' <- genMoveRand state'
                    state'' <- return $ updateGameState state' move'
                    case move' of
                      (Pass _) ->
                          return $ score state''
                      (StoneMove _) ->
                          run state'' (runCount + 1)
                      (Resign _) ->
                          error "runOneRandom encountered Resign"
          (StoneMove _) ->
              run state' (runCount + 1)
          (Resign _) ->
              error "runOneRandom encountered Resign"



genMoveRand :: (RandomGen g) => GameState -> Rand g Move
genMoveRand state =
    pickSane $ freeVertices (goban state)

    where
      pickSane :: (RandomGen g) => [Vertex] -> Rand g Move
      pickSane [] = return $ Pass (nextMoveColor state)
      pickSane moves = do
        p <- pick moves
        case sane p of
          True -> return $ StoneMove (Stone (p, color))
          False -> pickSane (moves \\ [p])

      sane p =
          (not (p `elem` (koBlocked state))) &&
          (not (isPotentialFullEye g color p)) &&
          (not (isSuicideVertex g color p))
      g = goban state
      color = nextMoveColor state


saneMoves :: GameState -> [Vertex]
saneMoves state =
    filter (not . (isSuicideVertex g color)) $
           filter (not . (isPotentialFullEye g color)) $
                      frees \\ (koBlocked state)
    where
      frees =
          if length (moveHistory state) > m
          then
              freeVertices g
          else
              freeNonEdgeVertices g

      g = goban state
      color = nextMoveColor state
      m = truncate $ sqrt (fromIntegral (sizeOfGoban g) :: Float)

-- saneNeighMoves :: GameState -> [Vertex]
-- saneNeighMoves state =
--     case moveHistory state of
--       [] ->
--           error "saneNeighbourMoves called with empty moveHistory"
--       moves ->
--           case last moves of
--             (StoneMove (Stone (p, _color))) ->
--                 filter (not . (isSuicideVertex g color)) $
--                        filter (not . (isEyeLike g color)) $
--                               moveCandidates \\ (koBlocked state)
--                 where
--                   moveCandidates =
--                       if null freeNs
--                       then frees
--                       else freeNs
--                   frees = freeVertices g
--                   freeNs = adjacentFree g p
--             (Pass _color) -> []
--             (Resign _color) -> []
--     where
--       g = goban state
--       color = nextMoveColor state


-- insaneMoves :: GameState -> [Vertex]
-- insaneMoves state =
--     filter (not . (isEyeLike g color)) $
--                (freeVertices g) \\ (koBlocked state)
--     where
--       g = goban state
--       color = nextMoveColor state

pick :: (RandomGen g) => [a] -> Rand g a
pick as = do
  i <- getRandomR (0, ((length as) - 1))
  return $ as !! i
