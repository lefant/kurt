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
      goban           :: !VectorGoban
    , koBlocked       :: ![Vertex]
    , moveHistory     :: ![Move]
    , komi            :: !Score
    , blackPrisoners  :: !Score
    , whitePrisoners  :: !Score
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


newGameState :: Int -> Score -> GameState
newGameState boardsize newKomi =
    GameState { goban = (newGoban boardsize)
              , koBlocked = []
              , moveHistory = []
              , komi = newKomi
              , blackPrisoners = 0
              , whitePrisoners = 0
              }
