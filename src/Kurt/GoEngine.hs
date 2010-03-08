{-# OPTIONS -O2 -Wall -Werror -Wwarn #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE BangPatterns #-}


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
                     , EngineState(..)
                     , newEngineState
                     ) where


import Control.Monad (liftM)
import Control.Monad.ST (ST, stToIO)
import System.Random.MWC (Gen, create, uniform)
import Data.Time.Clock ( UTCTime(..)
                       , picosecondsToDiffTime
                       , getCurrentTime
                       )
import Data.List ((\\))
-- import Data.List (sort)
-- import Text.Printf (printf)


import Data.Goban.GameState (GameState(..), newGameState, scoreGameState, updateGameState, nextMoveColor, nextMoves)
import Data.Goban.Goban (Move(..), Stone(..), Color, Vertex, Score)
import Data.Goban.STVector ()
import Data.Goban.Utils (winningScore, scoreToResult)


import Data.Tree.UCT.GameTree (UCTTreeLoc)
import Data.Tree.UCT

import Debug.Trace (trace)
import Data.Tree (rootLabel)
import Data.Tree.Zipper (tree)
import Data.Tree.UCT.GameTree (MoveNode(..))
-- import Data.Tree (drawTree)
-- import Data.Tree.Zipper (tree)

data EngineState = EngineState {
      getGameState    :: GameState
    , boardSize       :: !Int
    , getKomi         :: !Score
    , simulCount      :: !Int
    , timePerMove     :: !Int
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
                 , timePerMove = 1000 }





genMove :: EngineState -> Color -> IO Move
genMove eState color =
    -- if (null (saneMoves state)) || ((winningProb bestMove) < 0.15)
    if null $ nextMoves gState color
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
      initLoc = rootNode $ nextMoves gState color
      gState = getGameState eState
      thinkPicosecs =
          picosecondsToDiffTime
          $ fromIntegral (timePerMove eState) * 1000000000


uctLoop :: UCTTreeLoc Move -> GameState -> UTCTime -> IO Move
uctLoop !loc rootGameState deadline = do
  -- done <- return $ trace ("uctLoop debug tree\n\n\n" ++
  --       (drawTree $ fmap show $ tree loc)) False
  done <- return False
  (loc', path) <- return $ selectLeafPath policyUCB1 loc
  leafGameState <- return $ getLeafGameState rootGameState path
  -- rGen <- trace ("uctLoop leafGameState \n" ++ (showboard (goban $ leafGameState))) $ newStdGen
  -- FIXME: rave will also need a sequence of moves here
  rGen <- stToIO create
  score <- stToIO $ runOneRandom leafGameState rGen
  value <- return $ scoreToResult (thisMoveColor leafGameState) score
  loc'' <- return $ expandNode loc' $ nextMoves leafGameState (nextMoveColor leafGameState)
  loc''' <- return $ backpropagate value loc''
  now <- getCurrentTime
  timeIsUp <- return $ (now > deadline)
  (if (done || timeIsUp)
   then return $ bestMoveFromLoc loc''' rootGameState
   else uctLoop loc''' rootGameState deadline)




bestMoveFromLoc :: UCTTreeLoc Move -> GameState -> Move
bestMoveFromLoc loc state =
    case principalVariation loc of
      [] ->
          error "bestMoveFromLoc: principalVariation is empty"
      (node : _) ->
          if value < 0.15
          then
              if winningScore color (scoreGameState state)
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




runOneRandom :: GameState -> Gen s -> ST s Score
runOneRandom initState rGenInit =
    run initState 0 rGenInit
    where
      run :: GameState -> Int -> Gen s -> ST s Score
      run _ 1000 _ = return 0
      run state runCount rGen = do
        move <- genMoveRand state rGen
        state' <- return $ updateGameState state move
        case move of
          (Pass _) -> do
                    move' <- genMoveRand state' rGen
                    state'' <- return $ updateGameState state' move'
                    case move' of
                      (Pass _) ->
                          return $ scoreGameState state''
                      (StoneMove _) ->
                          run state'' (runCount + 1) rGen
                      (Resign _) ->
                          error "runOneRandom encountered Resign"
          (StoneMove _) ->
              run state' (runCount + 1) rGen
          (Resign _) ->
              error "runOneRandom encountered Resign"



-- genMoveRand :: 
genMoveRand :: GameState -> Gen s -> ST s Move
genMoveRand state rGen =
    pickSane $ freeVertices (goban state)
    where
      pickSane [] = return $ Pass (nextMoveColor state)
      pickSane [p] =
          return $ if isSaneMove' p
                   then StoneMove (Stone (p, color))
                   else Pass (nextMoveColor state)
      pickSane ps = do
        p <- pick ps rGen
        (if isSaneMove' p
         then return $ StoneMove (Stone (p, color))
         else pickSane (ps \\ [p]))

      isSaneMove' = isSaneMove (goban state) (koBlocked state) color
      color = nextMoveColor state


pick :: [Vertex] -> Gen s -> ST s Vertex
pick as rGen = do
  i <- liftM (`mod` ((length as) - 1)) $ uniform rGen
  -- i <- getRandomR (0, ((length as) - 1))
  return $ as !! i




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
