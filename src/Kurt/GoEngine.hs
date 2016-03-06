-- Move generator logic

module Kurt.GoEngine ( genMove
                     , simulatePlayout
                     , EngineState(..)
                     , newEngineState
                     , updateEngineState
                     , newUctTree
                     ) where

import           Control.Arrow               (second)
import           Control.Monad               (liftM)
import           Control.Monad.Primitive     (PrimState)
import           Control.Monad.ST            (ST, runST, stToIO)
import           Control.Parallel.Strategies (parMap, rdeepseq)
import           Data.List                   ((\\))
import qualified Data.Map                    as M (map)
import           Data.Maybe                  (fromMaybe)
import           Data.Time.Clock             (UTCTime (..), getCurrentTime,
                                              picosecondsToDiffTime)
import           Data.Tree                   (rootLabel)
import           Data.Tree.Zipper            (findChild, fromTree, hasChildren,
                                              tree)
import           System.Random.MWC           (Gen, Seed, restore, save, uniform,
                                              withSystemRandom)

import           Data.Goban.GameState
import           Data.Goban.Types            (Color (..), Move (..), Score,
                                              Stone (..), Vertex)
import           Data.Goban.Utils            (rateScore, winningScore)
import           Kurt.Config

import           Data.Tree.UCT
import           Data.Tree.UCT.GameTree      (MoveNode (..), RaveMap,
                                              UCTTreeLoc, newMoveNode,
                                              newRaveMap)

import           Debug.TraceOrId             (trace)

-- import Data.Tree (drawTree)

data EngineState = EngineState {
      getGameState :: !GameState
    , getUctTree   :: !(UCTTreeLoc Move)
    , getRaveMap   :: !(RaveMap Move)
    , boardSize    :: !Int
    , getKomi      :: !Score
    , getConfig    :: !KurtConfig
    }

type LoopState = (UCTTreeLoc Move, RaveMap Move)

-- result from playout: score, playedMoves, path to startnode in tree
type Result = (Score, [Move], [Move])
-- request for playout: gamestate, path to startnode in tree, seed
type Request = (GameState, [Move], Seed)


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
  (if null moves
   then
       if winningScore color score
       then return (Pass color, eState)
       else return (Resign color, eState)
   else (do
          seed <- withSystemRandom (save :: Gen (PrimState IO) -> IO Seed)
          (loc', raveMap') <- runUCT loc gState raveMap config deadline seed
          let eState' = eState { getUctTree = loc', getRaveMap = raveMap' }
          return (bestMoveFromLoc loc' (getState gState) score, eState')))
    where
      config = getConfig eState
      gState = getGameState eState
      loc = getUctTree eState
      raveMap = M.map (second ((1 +) . (`div` 2))) $ getRaveMap eState
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
       -> UTCTime
       -> Seed
       -> IO LoopState
runUCT initLoc rootGameState initRaveMap config deadline seed00 = do
  uctLoop stateStream0 0
    where
      uctLoop :: [LoopState] -> Int -> IO LoopState
      uctLoop [] _ = return (initLoc, initRaveMap)
      uctLoop (st : stateStream) !n = do
        _ <- return $! st
        let maxRuns = n >= (maxPlayouts config)
        now <- getCurrentTime
        let timeIsUp = (now > deadline)
        (if maxRuns || timeIsUp
         then return st
         else uctLoop stateStream (n + 1))

      stateStream0 = loop0 seed00 (initLoc, initRaveMap)

      loop0 :: Seed -> LoopState -> [LoopState]
      loop0 seed0 st0 =
          map (\(_, st, _) -> st) $ iterate loop (seed0, st0, [])
          where
            loop (seed, st, results0) =
                (seed', st'', results)
                where
                  st'' = updater st' r
                  r : results = results0 ++ (parMap rdeepseq runOne requests)
                  (st', seed', requests) = requestor st seed reqNeeded
                  reqNeeded = max 2 $ maxThreads config - length results0

      updater :: LoopState -> Result -> LoopState
      updater !st !res =
          updateTreeResult st res

      requestor :: LoopState -> Seed -> Int -> (LoopState, Seed, [Request])
      requestor !st0 seed0 !n =
          last $ take n $ iterate r (st0, seed0, [])
          where
            r :: (LoopState, Seed, [Request]) -> (LoopState, Seed, [Request])
            r (!st, seed, rs) = (st', seed', request : rs)
                where
                  seed' = incrSeed seed
                  st' = (loc, raveMap)
                  (_, raveMap) = st
                  request = (leafGameState, path, seed)
                  (loc, (leafGameState, path)) = nextNode st

      nextNode :: LoopState -> (UCTTreeLoc Move, (GameState, [Move]))
      nextNode (!loc, !raveMap) =
          (loc'', (leafGameState, path))
              where
                loc'' = backpropagate (\_x -> 0) updateNodeVisits $ expandNode loc' slHeu moves
                moves = nextMoves leafGameState $ nextMoveColor $ getState leafGameState
                leafGameState = getLeafGameState rootGameState path
                (loc', path) = selectLeafPath policy loc
                policy = policyRaveUCB1 (uctExplorationPercent config) (raveWeight config) raveMap
                slHeu = makeStonesAndLibertyHeuristic leafGameState config


updateTreeResult :: LoopState -> Result -> LoopState
updateTreeResult (!loc, !raveMap) (!score, !playedMoves, !path) =
    (loc', raveMap')
    where
      raveMap' = updateRaveMap raveMap (rateScore score) $ drop (length playedMoves `div` 3) playedMoves
      loc' = backpropagate (rateScore score) updateNodeValue $ getLeaf loc path


simulatePlayout :: GameState -> IO [Move]
simulatePlayout gState = do
  seed <- withSystemRandom (save :: Gen (PrimState IO) -> IO Seed)
  let gState' = getLeafGameState gState []
  (oneState, playedMoves) <- stToIO $ runOneRandom gState' seed
  let score = scoreGameState oneState
  trace ("simulatePlayout " ++ show score) $ return ()
  return $ reverse playedMoves


runOne :: Request -> Result
runOne (gameState, path, seed) =
    (score, playedMoves, path)
    where
      score = scoreGameState endState
      (endState, playedMoves) = runST $ runOneRandom gameState seed

runOneRandom :: GameState -> Seed -> ST s (GameState, [Move])
runOneRandom initState seed = do
  rGen <- restore seed
  run initState 0 rGen []
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

incrSeed :: Seed -> Seed
incrSeed !seed =
    runST $ do
      gen <- restore seed
      x <- uniform gen
      _ <- return $ x + (1 :: Int)
      seed' <- save gen
      return $! seed'
