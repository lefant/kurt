{-# OPTIONS -O2 -Wall -Werror -Wwarn #-}

{- |
   Module     : Data.Goban.GameState
   Copyright  : Copyright (C) 2010 Fabian Linzberger
   License    : GNU GPL, version 3 or above

   Maintainer : Fabian Linzberger <e@lefant.net>
   Stability  : experimental
   Portability: probably

GameState Implementation

-}

module Data.Goban.GameState ( GameState(..)
                            , newGameState
                            ) where


import Data.Goban.Goban
import Data.Goban.Vector (VectorGoban)


data GameState = GameState {
      goban           :: VectorGoban
     ,komi            :: Score
     ,koBlocked       :: [Vertex]
     ,moveHistory     :: [Move]
     ,blackPrisoners  :: Score
     ,whitePrisoners  :: Score
    }


instance Show GameState where
    show state =
        case moveHistory state of
          [] -> ""
          moves ->
              show $ last moves
              -- case last moves of
              --   (StoneMove (Stone ((x, y), color))) ->
              --       c ++ [(xToLetter x)] ++ (show y)
              --       where
              --         c = case color of
              --               Black -> "b "
              --               White -> "w "
              --   (Pass _color) -> "pass"
              --   (Resign _color) -> "resign"


newGameState :: GameState
newGameState = GameState {
                     goban = (newGoban 1)
                    ,komi = 0
                    ,koBlocked = []
                    ,moveHistory = []
                    ,blackPrisoners = 0
                    ,whitePrisoners = 0
                   }
