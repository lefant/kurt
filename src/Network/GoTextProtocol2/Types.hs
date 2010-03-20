{-# OPTIONS -O2 -Wall -Werror -Wwarn -XRankNTypes #-}

{- |
   Module     : Network.GoTextProtocol2.Types
   Copyright  : Copyright (C) 2010 Fabian Linzberger
   License    : GNU GPL, version 3 or above

   Maintainer : Fabian Linzberger <e@lefant.net>
   Stability  : experimental
   Portability: probably

Shared types for Go Text Protocol implementations.

-}

module Network.GoTextProtocol2.Types ( Id
                                     , Command(..)
                                     , Argument(..)
                                     ) where

import Data.Goban.Types (Move, Color)

type Id = Int

data Command = Command String [Argument]
               deriving (Show, Eq)

data Argument = IntArgument Int
              | StringArgument String
              | MoveArgument Move
              | ColorArgument Color
              | FloatArgument Float
              | TimeLeftArgument Int Int
              | MaybeKeyValueArgument (Maybe (String, Int))
                deriving (Show, Eq)
