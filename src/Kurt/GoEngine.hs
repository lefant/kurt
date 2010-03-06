{-# OPTIONS -O2 -Wall -Werror -Wwarn #-}

{- |
   Module     : Kurt.GoEngine
   Copyright  : Copyright (C) 2010 Fabian Linzberger
   License    : GNU GPL, version 3 or above

   Maintainer : Fabian Linzberger <e@lefant.net>
   Stability  : experimental
   Portability: probably

Move generator logic

-}

module Kurt.GoEngine ( genMove
                     , uctDebug
                     , EngineState(..)
                     , newEngineState
                     , updateGameState
                     , scoreGameState
                     , winningScore
                     , saneMoves
                     , thisMoveColor
                     ) where


import System.Random (RandomGen, newStdGen)
import Control.Monad.Random (RandomGen, evalRand, Rand, getRandomR)
import Data.Tree.Zipper (TreeLoc, fromTree, tree)
import Data.Time.Clock ( UTCTime(..)
                       , picosecondsToDiffTime
                       , getCurrentTime
                       )
import Data.List (partition, foldl', (\\))
-- import Data.List (sort)
-- import Data.Tree (Tree(..))
-- import Text.Printf (printf)


import Data.Goban.Goban
import Data.Goban.Utils
import Data.Goban.GameState

import Data.Tree.UCT.GameTree
import Data.Tree.UCT

-- import Debug.Trace (trace)


data EngineState = EngineState {
      getGameState    :: GameState
    , boardSize       :: Int
    , getKomi         :: Score
    , simulCount      :: Int
    , timePerMove     :: Int
     -- maybe this could have colorToMove?
    }

defaultKomi :: Score
defaultKomi = 7.5

defaultBoardSize :: Int
defaultBoardSize = 9

newEngineState :: EngineState
newEngineState = EngineState {
                   getGameState = newGameState defaultBoardSize defaultKomi
                 , boardSize = defaultBoardSize
                 , getKomi = defaultKomi
                 , simulCount = 1000
                 , timePerMove = 3000 }



instance UCTNode Move where
    -- we use estimated win rate of move as value for uct nodes
    initialMoveValue _ = 0.5
    updateBackpropagationValue _ = negate





genMove :: EngineState -> Color -> IO Move
genMove eState color =
    -- if (null (saneMoves state)) || ((winningProb bestMove) < 0.15)
    if null $ saneMoves (goban gState) color (koBlocked gState)
    then
        if winningScore color (scoreGameState gState)
        then return $ Pass color
        else return $ Resign color
    else
        initUct eState color
    where
      gState = getGameState eState

initUct :: EngineState -> Color -> IO Move
initUct eState color = do
  now <- getCurrentTime
  uctLoop initLoc gState $ UTCTime { utctDay = (utctDay now)
                            , utctDayTime =
                                thinkPicosecs + (utctDayTime now) }
    where
      initLoc = rootNode $ saneMoves (goban gState) color (koBlocked gState)
      gState = getGameState eState
      thinkPicosecs =
          picosecondsToDiffTime
          $ fromIntegral (timePerMove eState) * 1000000000


uctLoop :: UCTTreeLoc Move -> GameState -> UTCTime -> IO Move
uctLoop loc gState deadline = do
  undefined
  -- rGen <- newStdGen
  -- (loc', done) <- return $ evalRand (uctZipperDown loc) rGen
  -- now <- getCurrentTime
  -- timeIsUp <- return $ (now > deadline)
  -- (if (done || timeIsUp)
  --  then return $ bestMoveFromLoc loc'
  --  else uctLoop loc' deadline)

-- runUCT loc = do
--   (loc', moves) <- selectLeafPath loc
--   state' <- leafState state moves
--   value' <- simulation state'
--   loc'' <- expand loc'
--   loc''' <- backpropagate loc''
--   return top level node loc'''



  
bestMoveFromLoc :: UCTTreeLoc Move -> GameState -> Move
bestMoveFromLoc loc state =
    case principalVariation loc of
      [] ->
          error "bestMoveFromLoc: principalVariation is empty"
      ((move, value) : _) ->
          if value < 0.15
          then
              if winningScore color (scoreGameState bestState)
              then Pass color
              else Resign color
          else
              move
          where
            bestState = updateGameState state move
            color = thisMoveColor state



-- compute game state at the end of a move sequence by replaying it
leafState :: GameState -> [Move] -> GameState
leafState = foldl' updateGameState










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
                          return $ scoreGameState state''
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
















uctDebug :: (RandomGen g) => GameState -> g -> String
uctDebug _state _rGen =
    "uctDebug currently deactivated"
--       gfxString $ runUct state rGen

-- gfxString :: Tree (UctLabel GameState) -> String
-- gfxString t =
--     (
--      "INFLUENCE " ++
--      (concatMap influenceFromLabel alternateFirstMoves) ++
--      "\n" ++
--      "LABEL " ++
--      (concatMap visitsFromLabel alternateFirstMoves) ++
--      "\n" ++ 
--      "TRIANGLE" ++
--      (concatMap ((((++) " ") . (show . nodeState))) $ filter isDone alternateFirstMoves) ++
--      -- "\n" ++
--      -- "VAR " ++
--      -- (concatMap moveFromLabel $ principalVariation t) ++
--      "\n"
--     )
--     where
--       alternateFirstMoves =
--           map rootLabel $ take 15 $ reverse $ sort $ subForest t
    

-- influenceFromLabel :: (UctNode a) => UctLabel a -> String
-- influenceFromLabel label =
--     show (nodeState label) ++ " " ++
--     (printf "%.2f " (((winningProb label) - 0.5) * 2))

-- visitsFromLabel :: (UctNode a) => UctLabel a -> String
-- visitsFromLabel label =
--     show (nodeState label) ++ " " ++
--     if isDone label
--     then ""
--     else show (visits label) ++ " "







-- moveFromLabel :: UctLabel GameState -> String
-- moveFromLabel label =
--     case moveHistory state of
--       [] -> ""
--       moves ->
--           (show $ thisMoveColor state) ++ " " ++
--           (show $ last moves) ++ " "
--     where
--       state = nodeState label
