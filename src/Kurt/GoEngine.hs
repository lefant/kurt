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
                     ) where


import Control.Monad (liftM)
import Control.Monad.ST (ST, RealWorld, stToIO)
import System.Random.MWC (Gen, uniform, withSystemRandom, save, restore)
import Data.Time.Clock ( UTCTime(..)
                       , picosecondsToDiffTime
                       , getCurrentTime
                       )
import Data.List ((\\))
-- import qualified Data.Map as M (empty, insert)


import Kurt.Config
import Data.Goban.Types (Move(..), Stone(..), Color(..), Vertex, Score)
import Data.Goban.Utils (winningScore, rateScore)
import Data.Goban.GameState (GameState(..), newGameState, scoreGameState, updateGameState, getLeafGameState, makeStonesAndLibertyHeuristic, nextMoveColor, nextMoves, isSaneMove, freeVertices)


import Data.Tree.UCT.GameTree (MoveNode(..), UCTTreeLoc, RaveMap, newRaveMap, newMoveNode)
import Data.Tree.UCT

import Debug.TraceOrId (trace)
import Data.Tree (rootLabel)
import Data.Tree.Zipper (tree, fromTree)

data EngineState s = EngineState {
      getGameState    :: GameState s
    , getUctTree      :: !(UCTTreeLoc Move)
    , getRaveMap      :: !(RaveMap Move)
    , boardSize       :: !Int
    , getKomi         :: !Score
    , getConfig       :: !KurtConfig
    }




newEngineState :: KurtConfig -> ST s (EngineState s)
newEngineState config = do
  gs <- newGameState (initialBoardsize config) (initialKomi config)
  -- boardStr <- showGoban $ goban gs
  -- trace ("newEngineState" ++ boardStr) $ return ()
  return EngineState {
                   getGameState = gs
                 , getUctTree = fromTree $ newMoveNode
                                (trace "UCT tree root move accessed"
                                           (Move (Stone (25,25) Black)))
                                (0.5, 1)
                 , getRaveMap = newRaveMap
                 , boardSize = initialBoardsize config
                 , getKomi = initialKomi config
                 , getConfig = config
                 }




genMove :: EngineState RealWorld -> Color -> IO (Move, EngineState RealWorld)
genMove eState color = do
  now <- getCurrentTime
  let deadline = UTCTime { utctDay = utctDay now
                         , utctDayTime = thinkPicosecs + utctDayTime now }

  moves <- stToIO $ nextMoves gState color
  score <- stToIO $ scoreGameState gState
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
          slHeu <- stToIO $ makeStonesAndLibertyHeuristic gState config

          let loc' = expandNode loc slHeu moves

          seed <- withSystemRandom save
          rGen <- stToIO $ restore seed


          -- -- initialize rave map with random values to avoid all moves
          -- -- ranked equal in some situation
          -- initRaveMap <- stToIO $ foldM (u rGen) M.empty
          --                [ Move (Stone p c) | p <- (allVertices (boardsize gState)), c <- [Black, White]]


          (loc'', raveMap) <- runUCT loc' gState initRaveMap config rGen deadline
          -- (getUctC eState) (getRaveWeight eState) (getHeuWeights eState) rGen deadline (maxRuns eState)
          let eState' = eState { getUctTree = loc'', getRaveMap = raveMap }

          return $ (bestMoveFromLoc loc'' gState score, eState')))


    where
      config = getConfig eState

      gState = getGameState eState
      initRaveMap = newRaveMap

      -- u :: Gen RealWorld -> RaveMap Move -> Move -> ST RealWorld (RaveMap Move)
      -- u rGen' m move = do
      --   v <- uniform rGen'
      --   return $ M.insert move ((0.495 + v / 100), 1) m

      loc = fromTree $ newMoveNode
                       (trace "UCT tree root move accessed"
                                  (Move (Stone (25,25) Black)))
                       (0.5, 1)
      thinkPicosecs =
          picosecondsToDiffTime
          $ fromIntegral (maxTime config) * 1000000000





bestMoveFromLoc :: UCTTreeLoc Move -> GameState s -> Score -> Move
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
       -> GameState RealWorld
       -> RaveMap Move
       -> KurtConfig
       -> Gen RealWorld
       -> UTCTime
       -> IO (UCTTreeLoc Move, RaveMap Move)
runUCT initLoc rootGameState initRaveMap config rGen deadline  =
    uctLoop initLoc initRaveMap 0

    where
      uctLoop            :: UCTTreeLoc Move -> RaveMap Move -> Int
                         -> IO (UCTTreeLoc Move, RaveMap Move)
      uctLoop !loc !raveMap n
          | n >= (maxPlayouts config) = return (loc, raveMap)
          | otherwise    = do
        let done = False

        (loc', path) <- return $ selectLeafPath
                        (policyRaveUCB1 (uctExplorationPercent config) (raveWeight config) raveMap) loc
                        -- (policyUCB1 (uctExploration config)) loc

        leafGameState <- stToIO $ getLeafGameState rootGameState path

        slHeu <- stToIO $ makeStonesAndLibertyHeuristic leafGameState config

        moves <- stToIO $ nextMoves leafGameState $ nextMoveColor leafGameState

        let loc'' = expandNode loc' slHeu moves
        -- let loc'' = expandNode loc' constantHeuristic moves

        (score, playedMoves) <- stToIO $ runOneRandom leafGameState rGen

        let raveMap' = updateRaveMap raveMap (rateScore score) $ drop (length playedMoves `div` 3) playedMoves

        let loc''' = backpropagate (rateScore score) loc''

        now <- getCurrentTime
        let timeIsUp = (now > deadline)
        (if done || timeIsUp
         then return (loc''', raveMap')
         else uctLoop loc''' raveMap' (n + 1))





simulatePlayout :: GameState RealWorld -> IO [Move]
simulatePlayout gState = do
  seed <- withSystemRandom save
  rGen <- stToIO $ restore seed

  gState' <- stToIO $ getLeafGameState gState []

  (score, playedMoves) <- stToIO $ runOneRandom gState' rGen

  trace ("simulatePlayout " ++ show score) $ return ()

  return $ reverse playedMoves



runOneRandom :: GameState s -> Gen s -> ST s (Score, [Move])
runOneRandom initState rGenInit =
    run initState 0 rGenInit []
    where
      run :: GameState s -> Int -> Gen s -> [Move] -> ST s (Score, [Move])
      run _ 1000 _ _ = return (0, [])
      run state runCount rGen moves = do
        move <- genMoveRand state rGen
        state' <- updateGameState state move
        case move of
          (Pass passColor) -> do
                    move' <- genMoveRand state' rGen
                    state'' <- updateGameState state' move'
                    case move' of
                      (Pass _) -> do
                                 score <- scoreGameState state''
                                 return (score, moves)
                      sm@(Move _) ->
                          run state'' (runCount + 1) rGen (sm : (Pass passColor) : moves)
                      (Resign _) ->
                          error "runOneRandom encountered Resign"
          sm@(Move _) ->
              run state' (runCount + 1) rGen (sm : moves)
          (Resign _) ->
              error "runOneRandom encountered Resign"



genMoveRand :: GameState s -> Gen s -> ST s Move
genMoveRand state rGen =
    pickSane $ freeVertices state
    where
      pickSane [] =
           return $ Pass color
      pickSane [p] = do
        let stone = Stone p color
        sane <- isSaneMove state stone
        return (if sane
                then Move stone
                else Pass color)
      pickSane ps = do
        p <- pick ps rGen
        let stone = Stone p color
        sane <- isSaneMove state stone
        (if sane
         then return $ Move stone
         else pickSane (ps \\ [p]))

      color = nextMoveColor state


pick :: [Vertex] -> Gen s -> ST s Vertex
pick as rGen = do
  i <- liftM (`mod` (length as)) $ uniform rGen
  return $ as !! i


