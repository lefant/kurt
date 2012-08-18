{-# OPTIONS -Wall -Werror -Wwarn #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE DeriveDataTypeable #-}

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


import           Control.Arrow           (second)
import           Control.Monad           (liftM)
import           Control.Monad.Primitive (PrimState)
import           Control.Monad.ST        (RealWorld, ST, stToIO)
import           Data.List               ((\\))
import qualified Data.Map                as M
import           Data.Maybe              (fromMaybe)
import           Data.Time.Clock         (UTCTime (..), getCurrentTime,
                                          picosecondsToDiffTime)
import           System.Random.MWC       (Gen, Seed, restore, save, uniform,
                                          withSystemRandom)
-- import qualified Data.Map as M (empty, insert)
import           Data.Tree               (rootLabel)
import           Data.Tree.Zipper        (findChild, fromTree, hasChildren,
                                          tree)


import           Data.Goban.GameState
import           Data.Goban.Types        (Color (..), Move (..), Score,
                                          Stone (..), Vertex)
import           Data.Goban.Utils        (rateScore, winningScore)
import           Kurt.Config

import           Data.Tree.UCT
import           Data.Tree.UCT.GameTree  (MoveNode (..), RaveMap, UCTTreeLoc,
                                          newMoveNode, newRaveMap)

import           Debug.TraceOrId         (trace)

-- import Data.Tree (drawTree)

data EngineState = EngineState {
      getGameState    :: !GameState
    , getUctTree      :: !(UCTTreeLoc Move)
    , getRaveMap      :: !(RaveMap Move)
    , boardSize       :: !Int
    , getKomi         :: !Score
    , getConfig       :: !KurtConfig
    }




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
      loc'' = fromTree $ tree loc'
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
       then return (Pass color, eState)
       else return (Resign color, eState)
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

          return (bestMoveFromLoc loc' (getState gState) score, eState')))


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
runUCT initLoc rootGameState initRaveMap config rGen deadline =
  uctLoop initLoc initRaveMap 0
  where
    uctLoop :: UCTTreeLoc Move -> RaveMap Move -> Int
               -> IO (UCTTreeLoc Move, RaveMap Move)
    uctLoop !loc !raveMap n
      | n >= maxPlayouts config = return (loc, raveMap)
      | otherwise = do
        let done = False

        (loc', path) <- return $ selectLeafPath
                        (policyRaveUCB1 (uctExplorationPercent config) (raveWeight config) raveMap) loc
                        -- (policyUCB1 (uctExploration config)) loc

        let leafGameState = getLeafGameState rootGameState path
        let slHeu = makeStonesAndLibertyHeuristic leafGameState config

        let moves = nextMoves leafGameState $ nextMoveColor $ getState leafGameState

        let loc'' = expandNode loc' slHeu moves
        -- let loc'' = expandNode loc' constantHeuristic moves

        (oneState, playedMoves) <- stToIO $ runOneRandom leafGameState rGen

        let score = scoreGameState oneState

        let raveMap' = updateRaveMap raveMap (rateScore score) $ drop (length playedMoves `div` 3) playedMoves

        let loc''' = backpropagate (rateScore score) loc''

        now <- getCurrentTime
        let timeIsUp = (now > deadline)
        (if done || timeIsUp
         then return (loc''', raveMap')
         else uctLoop loc''' raveMap' (n + 1))





simulatePlayout :: GameState -> IO [Move]
simulatePlayout gState = do
  seed <- withSystemRandom (save :: Gen (PrimState IO) -> IO Seed)
  rGen <- stToIO $ restore seed

  let gState' = getLeafGameState gState []

  (oneState, playedMoves) <- stToIO $ runOneRandom gState' rGen
  let score = scoreGameState oneState

  trace ("simulatePlayout " ++ show score) $ return ()

  return $ reverse playedMoves



runOneRandom :: GameState -> Gen s -> ST s (GameState, [Move])
runOneRandom initState rGenInit =
    run initState 0 rGenInit []
    where
      run :: GameState -> Int -> Gen s -> [Move] -> ST s (GameState, [Move])
      run state 1000 _ moves = return (trace ("runOneRandom not done after 1000 moves " ++ show moves) state, [])
      run state runCount rGen moves = do
        move <- genMoveRand state rGen
        let state' = updateGameState state move
        case move of
          (Pass passColor) -> do
                    move' <- genMoveRand state' rGen
                    let state'' = updateGameState state' move'
                    case move' of
                      (Pass _) ->
                          return (state'', moves)
                      sm@(Move _) ->
                          run state'' (runCount + 1) rGen (sm : Pass passColor : moves)
                      (Resign _) ->
                          error "runOneRandom encountered Resign"
          sm@(Move _) ->
              run state' (runCount + 1) rGen (sm : moves)
          (Resign _) ->
              error "runOneRandom encountered Resign"



genMoveRand :: GameState -> Gen s -> ST s Move
genMoveRand state rGen =
    pickSane $ freeVertices $ getState state
    where
      pickSane [] =
           return $ Pass color
      pickSane [p] = do
        let stone = Stone p color
        let sane = isSaneMove state stone
        return (if sane
                then Move stone
                else Pass color)
      pickSane ps = do
        p <- pick ps rGen
        let stone = Stone p color
        let sane = isSaneMove state stone
        (if sane
         then return $ Move stone
         else pickSane (ps \\ [p]))
      color = nextMoveColor $ getState state


pick :: [Vertex] -> Gen s -> ST s Vertex
pick as rGen = do
  i <- liftM (`mod` length as) $ uniform rGen
  return $ as !! i
