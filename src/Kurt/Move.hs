{-# OPTIONS -O2 -Wall -Werror -Wwarn #-}

{- |
   Module     : Kurt.Move
   Copyright  : Copyright (C) 2010 Fabian Linzberger
   License    : GNU GPL, version 3 or above

   Maintainer : Fabian Linzberger <e@lefant.net>
   Stability  : experimental
   Portability: probably

Move generator logic

-}

module Kurt.Move ( genMove
                 , uctDebug
                 ) where

import Control.Concurrent.MVar (MVar(..), newMVar, takeMVar, swapMVar)
import Control.Concurrent (forkIO, threadDelay, killThread)

import System.Random (RandomGen, newStdGen)
import Control.Monad.Random (Rand, RandomGen, evalRand)
import Control.Monad (unless)
import Data.List (sort)
import Data.Tree (Tree(..))
import Data.Tree.Zipper (TreeLoc, fromTree, tree)
import Text.Printf (printf)

import Data.Goban.Utils
import Data.Goban (GameState(..), saneMoves, score, winningScore)
import Data.Tree.UCT




genMove :: GameState -> Color -> IO Move
genMove state color =
    -- if (null (saneMoves state)) || ((winningProb bestMove) < 0.15)
    if null (saneMoves state)
    then
        if winningScore color (score state)
        then return $ Pass color
        else return $ Resign color
    else
        genMoveIO state color

genMoveIO :: GameState -> Color -> IO Move
genMoveIO state color = do
    mvar <- newMVar $ StoneMove (Stone (head $ saneMoves state, color))
    uctThread <- forkIO $ initUct state mvar
    threadDelay $ (timePerMove state) * 1000
    move <- takeMVar mvar
    killThread uctThread
    return move


initUct :: GameState -> MVar Move -> IO ()
initUct initState mvar =
    uctLoop (fromTree $ makeNodeWithChildren initState) mvar

uctLoop :: TreeLoc (UctLabel GameState) -> MVar Move -> IO ()
uctLoop loc mvar = do
  rGen <- newStdGen
  (loc', done) <- return $ evalRand (uctZipperDown loc) rGen
  currentBestMove <- return $ bestMoveFromLoc loc'
  swapMVar mvar currentBestMove
  unless done $ uctLoop loc' mvar
  return ()

bestMoveFromLoc :: TreeLoc (UctLabel GameState) -> Move
bestMoveFromLoc loc =
    case moveHistory $
         nodeState $
         head $ principalVariation $
         tree loc of
      [] -> error "genMove: moveHistory of bestMove is empty"
      moves -> last moves



uctDebug :: (RandomGen g) => GameState -> g -> String
uctDebug state rGen =
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
