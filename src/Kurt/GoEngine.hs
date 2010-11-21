{-# OPTIONS -O2 -Wall -Werror -Wwarn #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE BangPatterns, DeriveDataTypeable #-}

{- |
   Module     : Kurt.GoEngine
   Copyright  : Copyright (C) 2010 Fabian Linzberger
   License    : GNU GPL, version 3 or above

   Maintainer : Fabian Linzberger <e@lefant.net>
   Stability  : experimental

Move generator logic

-}

module Kurt.GoEngine ( genMove
                     , simulatePlayout
                     , EngineState(..)
                     , newEngineState
                     , updateEngineState
                     , newUctTree
                     ) where


import Control.Arrow (second)
import Control.Monad (liftM)
import Control.Monad.ST (ST, RealWorld, stToIO)
import Control.Monad.Primitive (PrimState)
import System.Random.MWC (Gen, Seed, uniform, withSystemRandom, save, restore)
import Control.Concurrent.Chan.Strict (Chan, newChan, writeChan, getChanContents)
import Control.Concurrent (forkIO, threadDelay)
import Data.Time.Clock ( UTCTime(..)
                       , picosecondsToDiffTime
                       , getCurrentTime
                       )
import Data.List ((\\), foldl')
import Data.Maybe (fromMaybe)
import qualified Data.Map as M
-- import qualified Data.Map as M (empty, insert)
import Data.Tree (rootLabel)
import Data.Tree.Zipper (tree, fromTree, findChild, hasChildren, root)


import Kurt.Config
import Data.Goban.Types (Move(..), Stone(..), Color(..), Vertex, Score)
import Data.Goban.Utils (winningScore, rateScore)
import Data.Goban.GameState (GameState(..), GameStateST(..), GameStateStuff, newGameState, freezeGameStateST, scoreGameState, scoreGameStateST, updateGameState, updateGameStateST, getLeafGameStateST, makeStonesAndLibertyHeuristic, nextMoveColor, nextMoves, nextMovesST, isSaneMoveST, freeVertices)

import Data.Tree.UCT.GameTree (MoveNode(..), UCTTreeLoc, RaveMap, newRaveMap, newMoveNode)
import Data.Tree.UCT

import Debug.TraceOrId (trace)

-- import Data.Tree (drawTree)

data EngineState = EngineState {
      getGameState    :: !GameState
    , getUctTree      :: !(UCTTreeLoc Move)
    , getRaveMap      :: !(RaveMap Move)
    , boardSize       :: !Int
    , getKomi         :: !Score
    , getConfig       :: !KurtConfig
    }

-- result from playout thread: score, playedMoves, path to startnode in tree
type Result = (Score, [Move], [Move])



newEngineState :: KurtConfig -> EngineState
newEngineState config =
    EngineState { getGameState =
                      newGameState (initialBoardsize config) (initialKomi config)
                , getUctTree = newUctTree
                , getRaveMap = newRaveMap
                , boardSize = initialBoardsize config
                , getKomi = initialKomi config
                , getConfig = config
                }

newUctTree :: UCTTreeLoc Move
newUctTree =
    fromTree $ newMoveNode
            (trace "UCT tree root move accessed"
             (Move (Stone (25,25) White)))
            (0.5, 1)

updateEngineState :: EngineState -> Move -> EngineState
updateEngineState eState move =
    eState { getGameState = gState', getUctTree = loc' }
    where
      gState' = updateGameState gState move
      gState = getGameState eState
      loc' = case move of
               (Resign _) -> loc
               _otherwise ->
                   if hasChildren loc
                   then selectSubtree loc move
                   else newUctTree
      loc = getUctTree eState

selectSubtree :: UCTTreeLoc Move -> Move -> UCTTreeLoc Move
selectSubtree loc move =
    loc''
    where
      loc'' = fromTree $ tree $ loc'
      loc' =
          fromMaybe newUctTree
          $ findChild ((move ==) . nodeMove . rootLabel) loc



genMove :: EngineState -> Color -> IO (Move, EngineState)
genMove eState color = do
  now <- getCurrentTime
  let deadline = UTCTime { utctDay = utctDay now
                         , utctDayTime = thinkPicosecs + utctDayTime now }

  let moves = nextMoves gState color
  let score = scoreGameState gState
  -- boardStr <- stToIO $ showGoban $ goban gState
  -- trace ("genMove" ++ boardStr) $ return ()
  -- trace ("genMove freeVertices: " ++ show (freeVertices gState)) $ return ()
  -- trace ("genMove moves: " ++ show moves) $ return ()
  -- trace ("genMove score: " ++ show score) $ return ()
  (if null moves
   then
       if winningScore color score
       then return $ (Pass color, eState)
       else return $ (Resign color, eState)
   else (do
          seed <- withSystemRandom (save :: Gen (PrimState IO) -> IO Seed)
          rGen <- stToIO $ restore seed


          -- -- initialize rave map with random values to avoid all moves
          -- -- ranked equal in some situation
          -- initRaveMap <- stToIO $ foldM (u rGen) M.empty
          --                [ Move (Stone p c) | p <- (allVertices (boardsize gState)), c <- [Black, White]]


          (loc', raveMap') <- runUCT loc gState raveMap config rGen deadline
          -- (getUctC eState) (getRaveWeight eState) (getHeuWeights eState) rGen deadline (maxRuns eState)
          let eState' = eState { getUctTree = loc', getRaveMap = raveMap' }

          return $ (bestMoveFromLoc loc' (getState gState) score, eState')))


    where
      config = getConfig eState

      gState = getGameState eState
      loc = getUctTree eState
      raveMap = M.map (second ((1 +) . (`div` 2))) $ getRaveMap eState

      -- u :: Gen RealWorld -> RaveMap Move -> Move -> ST RealWorld (RaveMap Move)
      -- u rGen' m move = do
      --   v <- uniform rGen'
      --   return $ M.insert move ((0.495 + v / 100), 1) m

      thinkPicosecs =
          picosecondsToDiffTime
          $ fromIntegral (maxTime config) * 1000000000





bestMoveFromLoc :: UCTTreeLoc Move -> GameStateStuff -> Score -> Move
bestMoveFromLoc loc state score =
    case principalVariation loc of
      [] ->
          error "bestMoveFromLoc: principalVariation is empty"
      (node : _) ->
          if value < 0.1
          then
              if winningScore color score
              then
                  trace ("bestMoveFromLoc pass " ++ show node)
                  Pass color
              else
                  trace ("bestMoveFromLoc resign " ++ show node)
                  Resign color
          else
              trace ("total sims: " ++ show (nodeVisits$rootLabel$tree$loc)
                     ++ " best: " ++ show node
                     ++ "\n")
                     -- ++ (drawTree $ fmap show $ tree loc)
              move
          where
            move = nodeMove node
            value = nodeValue node
            color = nextMoveColor state




runUCT :: UCTTreeLoc Move
       -> GameState
       -> RaveMap Move
       -> KurtConfig
       -> Gen RealWorld
       -> UTCTime
       -> IO (UCTTreeLoc Move, RaveMap Move)
runUCT initLoc rootGameState initRaveMap config rGen deadline = do
  resultQ <- newChan
  uctLoop initLoc initRaveMap 0 0 resultQ
    where
      -- strategy:
      --
      -- check if time is up or n >= (maxPlayouts config)
      --   flush return queue using updateTree
      --   return
      -- check if there is a pending result
      --   flush return queue using updateTree
      --   loop
      -- check if maxThread reached
      --   sleep 10ms
      --   loop
      -- compute job, enqueue

      uctLoop            :: UCTTreeLoc Move -> RaveMap Move -> Int -> Int -> Chan Result
                         -> IO (UCTTreeLoc Move, RaveMap Move)
      uctLoop !loc !raveMap n threadCount resultQ = do
        results <- getChanContents resultQ
        let (loc', raveMap') = foldl' updateTree (loc, raveMap) results
        let maxRuns = n >= (maxPlayouts config)
        now <- getCurrentTime
        let timeIsUp = (now > deadline)
        (if maxRuns || timeIsUp
         then return (loc', raveMap')
         else
           (do
               (loc'', path, leafGameState) <- nextNode loc' raveMap'

               forkIO $ runOneRandomIO leafGameState path resultQ
               threadDelay 10000
               uctLoop loc'' raveMap' (n + 1) threadCount resultQ))

        where
          updateTree :: (UCTTreeLoc Move, RaveMap Move) -> Result
                    -> (UCTTreeLoc Move, RaveMap Move)
          updateTree (loc, raveMap) (score, playedMoves, path) =
            (loc', raveMap')
            where
              raveMap' = updateRaveMap raveMap (rateScore score) $ drop (length playedMoves `div` 3) playedMoves
              loc' = backpropagate (rateScore score) $ getLeaf loc path

          nextNode :: UCTTreeLoc Move -> RaveMap Move -> IO (UCTTreeLoc Move, [Move], GameStateST RealWorld)
          nextNode loc raveMap = do
            (loc', path) <- return $ selectLeafPath
                            (policyRaveUCB1 (uctExplorationPercent config) (raveWeight config) raveMap) loc
                            -- (policyUCB1 (uctExploration config)) loc

            leafGameStateST <- stToIO $ getLeafGameStateST rootGameState path

            leafGameState <- stToIO $ freezeGameStateST leafGameStateST
            let slHeu = makeStonesAndLibertyHeuristic leafGameState config

            moves <- stToIO $ nextMovesST leafGameStateST $ nextMoveColor $ getStateST leafGameStateST

            let loc'' = root $ expandNode loc' slHeu moves
            -- let loc'' = expandNode loc' constantHeuristic moves

            return (loc'', path, leafGameStateST)


runOneRandomIO :: GameStateST RealWorld -> [Move] -> Chan Result -> IO ()
runOneRandomIO gameStateST path resultQ = do
  seed <- withSystemRandom (save :: Gen (PrimState IO) -> IO Seed)
  rGen <- stToIO $ restore seed
  (endState, playedMoves) <- stToIO $ runOneRandom gameStateST rGen
  score <- stToIO $ scoreGameStateST endState
  writeChan resultQ (score, playedMoves, path)


simulatePlayout :: GameState -> IO [Move]
simulatePlayout gState = do
  seed <- withSystemRandom (save :: Gen (PrimState IO) -> IO Seed)
  rGen <- stToIO $ restore seed

  gState' <- stToIO $ getLeafGameStateST gState []

  (oneState, playedMoves) <- stToIO $ runOneRandom gState' rGen
  score <- stToIO $ scoreGameStateST oneState

  trace ("simulatePlayout " ++ show score) $ return ()

  return $ reverse playedMoves



runOneRandom :: GameStateST s -> Gen s -> ST s (GameStateST s, [Move])
runOneRandom initState rGenInit =
    run initState 0 rGenInit []
    where
      run :: GameStateST s -> Int -> Gen s -> [Move] -> ST s (GameStateST s, [Move])
      run state 1000 _ moves = return (trace ("runOneRandom not done after 1000 moves " ++ show moves) state, [])
      run state runCount rGen moves = do
        move <- genMoveRand state rGen
        state' <- updateGameStateST state move
        case move of
          (Pass passColor) -> do
                    move' <- genMoveRand state' rGen
                    state'' <- updateGameStateST state' move'
                    case move' of
                      (Pass _) ->
                          return (state'', moves)
                      sm@(Move _) ->
                          run state'' (runCount + 1) rGen (sm : (Pass passColor) : moves)
                      (Resign _) ->
                          error "runOneRandom encountered Resign"
          sm@(Move _) ->
              run state' (runCount + 1) rGen (sm : moves)
          (Resign _) ->
              error "runOneRandom encountered Resign"



genMoveRand :: GameStateST s -> Gen s -> ST s Move
genMoveRand state rGen =
    pickSane $ freeVertices $ getStateST state
    where
      pickSane [] =
           return $ Pass color
      pickSane [p] = do
        let stone = Stone p color
        sane <- isSaneMoveST state stone
        return (if sane
                then Move stone
                else Pass color)
      pickSane ps = do
        p <- pick ps rGen
        let stone = Stone p color
        sane <- isSaneMoveST state stone
        (if sane
         then return $ Move stone
         else pickSane (ps \\ [p]))

      color = nextMoveColor $ getStateST state


pick :: [Vertex] -> Gen s -> ST s Vertex
pick as rGen = do
  i <- liftM (`mod` (length as)) $ uniform rGen
  return $ as !! i
