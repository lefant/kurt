{-# OPTIONS -O2 -Wall -Werror -Wwarn -XRankNTypes #-}

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

import Data.Goban (Move, Color)

type Id = Int

data Command = Command String [Argument]
               deriving (Show, Eq)

data Argument = IntArgument Int
              | StringArgument String
              | MoveArgument Move
              | ColorArgument Color
              | FloatArgument Float
                deriving (Show, Eq)
