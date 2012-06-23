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


import System.Console.CmdArgs.Implicit

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


kurtDefaultConfig :: KurtConfig
kurtDefaultConfig = KurtConfig {
               maxPlayouts = def &= typ "INT" &= opt (100000 :: Int)
                             &= help "Max simulations run during move generation"
             , maxTime = def &= typ "INT" &= opt (5000 :: Int)
                         &= help "Max time used during move generation (ms)"
             , uctExplorationPercent = def &= typ "INT" &= opt (10 :: Int)
                                &= help "Exploration constant used in UCT formula (percent)"
             , raveWeight = def &= typ "INT" &= opt (20 :: Int)
                                &= help "Weight used for RAVE in UCT-RAVE formula"
             , hCaptureWeight = def &= typ "INT" &= opt (25 :: Int)
                                &= help "Weight used for captures in heuristic"
             , hMinLibertiesWeight = def &= typ "INT" &= opt (7 :: Int)
                                &= help "Weight used for minimum liberties in heuristic"
             , hLibertiesWeight = def &= typ "INT" &= opt (3 :: Int)
                                &= help "Weight used for liberties in heuristic"
             , hChainCountWeight = def &= typ "INT" &= opt (1 :: Int)
                                &= help "Weight used for group count in heuristic"
             , hCenterWeight = def &= typ "INT" &= opt (5 :: Int)
                                &= help "Weight used for center moves in heuristic"
             , initialKomi = def &= typ "FLOAT" &= opt (7.5 :: Float)
                             &= help "Initial komi value (usually overridden via GTP)"
             , initialBoardsize = def &= typ "INT" &= opt (9 :: Int)
                             &= help "Initial boardsize (usually overridden via GTP)"
             } &= summary "Kurt - Computer Go program. Implemented as a GTP server to be run from a GTP client like gogui or kgsGTP."


--             } &= prog "kurt" &= help "Computer Go program. Implemented as a GTP server to be run from a GTP client like gogui or kgsGTP."

