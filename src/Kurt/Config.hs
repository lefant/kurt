{-# OPTIONS -O2 -Wall -Werror -Wwarn #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE DeriveDataTypeable #-}

{- |
   Module     : Kurt.Config
   Copyright  : Copyright (C) 2010 Fabian Linzberger
   License    : GNU GPL, version 3 or above

   Maintainer : Fabian Linzberger <e@lefant.net>
   Stability  : experimental

Program configuration

-}

module Kurt.Config ( KurtConfig(..)
                   , kurtDefaultConfig
                   ) where


import System.Console.CmdArgs

import Data.Goban.Types (Score)


data KurtConfig = KurtConfig { maxPlayouts           :: Int
                             , maxTime               :: Int
                             , uctExplorationPercent :: Int
                             , raveWeight            :: Int
                             , hCaptureWeight        :: Int
                             , hMinLibertiesWeight   :: Int
                             , hLibertiesWeight      :: Int
                             , hChainCountWeight     :: Int
                             , hCenterWeight         :: Int
                             , initialKomi           :: Score
                             , initialBoardsize      :: Int
                             }
                  deriving (Show, Eq, Data, Typeable)


kurtDefaultConfig :: Mode KurtConfig
kurtDefaultConfig = mode $ KurtConfig {
               maxPlayouts = 100000 &= typ "INT"
                             & text "Max simulations run during move generation"
             , maxTime = 5000 &= typ "INT"
                         & text "Max time used during move generation (ms)"
             , uctExplorationPercent = 10 &= typ "INT"
                                & text "Exploration constant used in UCT formula (percent)"
             , raveWeight = 20 &= typ "INT"
                                & text "Weight used for RAVE in UCT-RAVE formula"
             , hCaptureWeight = 25 &= typ "INT"
                                & text "Weight used for captures in heuristic"
             , hMinLibertiesWeight = 7 &= typ "INT"
                                & text "Weight used for minimum liberties in heuristic"
             , hLibertiesWeight = 3 &= typ "INT"
                                & text "Weight used for liberties in heuristic"
             , hChainCountWeight = 1 &= typ "INT"
                                & text "Weight used for group count in heuristic"
             , hCenterWeight = 5 &= typ "INT"
                                & text "Weight used for center moves in heuristic"
             , initialKomi = 7.5 &= typ "FLOAT"
                             & text "Initial komi value (usually overridden via GTP)"
             , initialBoardsize = 9 &= typ "INT"
                             & text "Initial boardsize (usually overridden via GTP)"
             } &= prog "kurt" & text "Computer Go program. Implemented as a GTP server to be run from a GTP client like gogui or kgsGTP."

