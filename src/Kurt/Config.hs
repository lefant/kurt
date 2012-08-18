{-# OPTIONS -Wall -Werror -Wwarn #-}
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


import           System.Console.CmdArgs.Implicit

import           Data.Goban.Types                (Score)


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


kurtDefaultConfig :: KurtConfig
kurtDefaultConfig = KurtConfig {
               maxPlayouts = (100000 :: Int) &= typ "INT"
                             &= help "Max simulations run during move generation"
             , maxTime = (5000 :: Int) &= typ "INT"
                         &= help "Max time used during move generation (ms)"
             , uctExplorationPercent = def &= opt (10 :: Int) &= typ "INT"
                                       &= help "Exploration constant used in UCT formula (percent)"
             , raveWeight = (20 :: Int) &= typ "INT"
                                &= help "Weight used for RAVE in UCT-RAVE formula"
             , hCaptureWeight = (25 :: Int) &= typ "INT"
                                &= help "Weight used for captures in heuristic"
             , hMinLibertiesWeight = (7 :: Int) &= typ "INT"
                                &= help "Weight used for minimum liberties in heuristic"
             , hLibertiesWeight = (3 :: Int) &= typ "INT"
                                &= help "Weight used for liberties in heuristic"
             , hChainCountWeight = (1 :: Int) &= typ "INT"
                                &= help "Weight used for group count in heuristic"
             , hCenterWeight = (5 :: Int) &= typ "INT"
                                &= help "Weight used for center moves in heuristic"
             , initialKomi = (7.5 :: Float) &= typ "FLOAT"
                             &= help "Initial komi value (usually overridden via GTP)"
             , initialBoardsize = (9 :: Int) &= typ "INT"
                             &= help "Initial boardsize (usually overridden via GTP)"
             }
                    &= program "kurt"
                    &= summary "Computer Go program. Implemented as a GTP server to be run from a GTP client like gogui or kgsGTP."
