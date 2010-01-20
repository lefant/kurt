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
   Module     : Data.Goban
   Copyright  : Copyright (C) 2010 Fabian Linzberger
   License    : GNU GPL, version 3 or above

   Maintainer : Fabian Linzberger <e@lefant.net>
   Stability  : experimental
   Portability: probably

Goban Implementation

-}

module Data.Goban (
                   GameState(..)
                  ,defaultGameState
                  ,Move(..)
                  ,Color(..)
                  ,Stone(..)
                  ,Vertex
                  ,Score
                  ,updateGameState
                  ) where

import Kurt.Utils (xToLetter)


data GameState = GameState {
      komi            :: Score
     ,boardsize       :: Int
     ,toMove          :: Color
     ,stones          :: [Stone]
     ,moveHistory     :: [Move]
     ,blackPrisoners  :: Score
     ,whitePrisoners  :: Score
    } deriving (Show, Eq)

defaultGameState :: GameState
defaultGameState = GameState {
                     komi = 0
                    ,boardsize = 1
                    ,toMove = Black
                    ,stones = []
                    ,moveHistory = []
                    ,blackPrisoners = 0
                    ,whitePrisoners = 0
                   }


data Move = StoneMove Stone
          | Pass Color
            deriving (Eq)

instance Show Move where
    show (StoneMove (Stone ((x, y), _color))) =
        [(xToLetter x)] ++ (show y)
    show (Pass _color) = "pass"


newtype Stone = Stone (Vertex, Color)
    deriving (Show, Eq)

data Color = Black
           | White
             deriving (Show, Eq)

type Vertex = (Int, Int)

type Score = Float


updateGameState :: GameState -> Move -> GameState
updateGameState state move =
    if (moveColor move) /= currentToMove
    then error "updateGameState: move by wrong color received"
    else state'
    where
      state' =
          case move of
            StoneMove stone ->
                state {
                     toMove = toMove'
                    ,stones = (stone : (stones state))
                    ,moveHistory = (moveHistory state) ++ [move]
                    ,blackPrisoners =
                        (blackPrisoners state) + blackPrisoners'
                    ,whitePrisoners =
                        (whitePrisoners state) + whitePrisoners'
                    }
            Pass _color ->
                state {
                     toMove = toMove'
                    ,moveHistory = (moveHistory state) ++ [move]
                    }

      currentToMove = toMove state
      toMove' = case currentToMove of
                  Black -> White
                  White -> Black
      blackPrisoners' = 0
      whitePrisoners' = 0



moveColor :: Move -> Color
moveColor (StoneMove (Stone (_vertex, color))) = color
moveColor (Pass color) = color


