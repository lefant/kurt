{-# OPTIONS -O2 -Wall -Werror -Wwarn #-}

{-
Copyright (C) 2010 Fabian Linzberger <e@lefant.net>

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.
-}

{- |
   Module     : Kurt.Move
   Copyright  : Copyright (C) 2010 Fabian Linzberger
   License    : GNU GPL, version 3 or above

   Maintainer : Fabian Linzberger <e@lefant.net>
   Stability  : experimental
   Portability: probably


Move generator logic

-}

module Kurt.Move (
                  genMove
                 ,uctDebug
                 ) where


import System.Random (RandomGen)
import Data.List (sort)
import Data.Tree (Tree(..))
import Debug.Trace (trace)
import Text.Printf (printf)

import Data.Goban.Utils
import Data.Goban (GameState(..), saneMoves, score, winningScore, thisMoveColor)
import Data.Tree.UCT





genMove :: (RandomGen g) => GameState -> Color -> g -> Move
genMove state color rGen =
    if (null (saneMoves state)) || ((winningProb bestMove) < 0.15)
    then
        if winningScore color (score state)
        then Pass color
        else Resign color
    else
        case moveHistory $ nodeState bestMove of
          [] -> error "genMove: moveHistory of bestMove is empty"
          moves -> last moves
    where
      bestMove =
          head pv
      pv =
          trace ("genMove: " ++ (gfxString t))
          principalVariation t
      t = runUct state rGen


runUct :: (RandomGen g) => GameState -> g -> Tree (UctLabel GameState)
runUct state rGen =
    uct state (simulCount state) rGen


uctDebug :: (RandomGen g) => GameState -> g -> String
uctDebug state rGen =
      gfxString $ runUct state rGen

gfxString :: Tree (UctLabel GameState) -> String
gfxString t =
    (
     "INFLUENCE " ++
     (concatMap influenceFromLabel alternateFirstMoves) ++
     "\n" ++
     "LABEL " ++
     (concatMap visitsFromLabel alternateFirstMoves) ++
     "\n" ++ 
     "VAR " ++
     (concatMap moveFromLabel $ principalVariation t) ++
     "\n"
    )
    where
      alternateFirstMoves =
          map rootLabel $ take 15 $ reverse $ sort $ subForest t
    

influenceFromLabel :: (UctNode a) => UctLabel a -> String
influenceFromLabel label =
    show (nodeState label) ++ " " ++
    (printf "%.2f " (((winningProb label) - 0.5) * 2))

visitsFromLabel :: (UctNode a) => UctLabel a -> String
visitsFromLabel label =
    show (nodeState label) ++ " " ++
    show (visits label) ++ " "

moveFromLabel :: UctLabel GameState -> String
moveFromLabel label =
    case moveHistory state of
      [] -> ""
      moves ->
          (show $ thisMoveColor state) ++ " " ++
          (show $ last moves) ++ " "
    where
      state = nodeState label
