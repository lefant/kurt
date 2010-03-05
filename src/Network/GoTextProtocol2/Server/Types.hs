{-# OPTIONS -O2 -Wall -Werror -Wwarn -XRankNTypes #-}

{- |
   Module     : Network.GoTextProtocol2.Server.Types
   Copyright  : Copyright (C) 2010 Fabian Linzberger
   License    : GNU GPL, version 3 or above

   Maintainer : Fabian Linzberger <e@lefant.net>
   Stability  : experimental
   Portability: probably

Shared types.

-}

module Network.GoTextProtocol2.Server.Types (
                                             Id
                                            ,Command(..)
                                            ,Argument(..)
                                            ) where

import Data.Goban.Goban (Move, Color)

type Id = Int

data Command = Command String [Argument]
               deriving (Show, Eq)

data Argument = IntArgument Int
              | StringArgument String
              | MoveArgument Move
              | ColorArgument Color
              | FloatArgument Float
              | TimeLeftArgument (Int, Int)
                deriving (Show, Eq)
