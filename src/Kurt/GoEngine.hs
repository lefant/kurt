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
                     ) where

import Data.Time.Clock ( UTCTime(..)
                       , picosecondsToDiffTime
                       , getCurrentTime
                       )

import System.Random (RandomGen, newStdGen)
import Control.Monad.Random (RandomGen, evalRand)
-- import Data.List (sort)
-- import Data.Tree (Tree(..))
import Data.Tree.Zipper (TreeLoc, fromTree, tree)
-- import Text.Printf (printf)

import Data.Goban.Goban
import Data.Goban (GameState(..), saneMoves, score, winningScore, thisMoveColor)
import Data.Tree.UCT

import Debug.Trace (trace)



genMove :: GameState -> Color -> IO Move
genMove state color =
    -- if (null (saneMoves state)) || ((winningProb bestMove) < 0.15)
    if null (saneMoves state)
    then
        if winningScore color (score state)
        then return $ Pass color
        else return $ Resign color
    else
        initUct state

initUct :: GameState -> IO Move
initUct initState = do
  now <- getCurrentTime
  uctLoop initLoc $ UTCTime { utctDay = (utctDay now)
                            , utctDayTime =
                                trace ("deadline: "
                                       ++ (show ( thinkPicosecs
                                                , (utctDayTime now)
                                                , thinkPicosecs+(utctDayTime now)
                                                )))
                                thinkPicosecs + (utctDayTime now) }
    where
      -- here we should probably first call our game specific code and
      -- then pass the result to uct instead of making uct initialize
      -- itself via the game specific code
      initLoc =
          fromTree $ makeNodeWithChildren initState
      thinkPicosecs =
          picosecondsToDiffTime
          $ fromIntegral (timePerMove initState) * 1000000000

uctLoop :: TreeLoc (UctLabel GameState) -> UTCTime -> IO Move
uctLoop loc deadline = do
  rGen <- newStdGen
  (loc', done) <- return $ evalRand (uctZipperDown loc) rGen
  now <- getCurrentTime
  timeIsUp <- return $ (now > deadline)
  (if (done || timeIsUp)
   then return $ bestMoveFromLoc loc'
   else uctLoop loc' deadline)

  
bestMoveFromLoc :: TreeLoc (UctLabel GameState) -> Move
bestMoveFromLoc loc =
    case principalVariation $ tree loc of
      [] ->
          error "bestMoveFromLoc: principalVariation is empty"
      (bestNode : _) ->
          if winningProb bestNode < 0.15
          then
              if winningScore color (score state)
              then Pass color
              else Resign color
          else
              case moveHistory state of
                [] -> error "genMove: moveHistory of bestMove is empty"
                moves ->
                    last moves
          where
            state = nodeState bestNode
            color = thisMoveColor state


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
