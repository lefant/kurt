{-# OPTIONS -Wall -Werror -Wwarn #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
   Module     : Data.Goban.GameState
   Copyright  : Copyright (C) 2010 Fabian Linzberger
   License    : GNU GPL, version 3 or above

   Maintainer : Fabian Linzberger <e@lefant.net>
   Stability  : experimental
   Portability: probably

Go GameState Implementation

-}

module Data.Goban.GameState ( GameState(..)
                            , GameStateStuff(..)

                            , newGameState
                            , updateGameState
                            , nextMoves
                            , scoreGameState
                            , showGameState

                            , getLeafGameState
                            , isSaneMove

                            , centerHeuristic
                            , makeStonesAndLibertyHeuristic

                            , freeVertices
                            , thisMoveColor
                            , nextMoveColor
                            ) where


import qualified Data.Set as S
import qualified Data.List as L (foldl')

import Kurt.Config
import Data.Goban.Types
import Data.Tree.UCT (UCTHeuristic)
import Data.Tree.UCT.GameTree (Value)
import Data.Goban.Utils
import Data.Goban.Incremental
import Data.Goban.ZobristHash (ZHash, updateHash)


import Debug.TraceOrId (trace)



data GameState = GameState { getGoban :: !GobanMap
                           , getState :: !GameStateStuff
                           }

data GameStateStuff = GameStateStuff { chains          :: !ChainMap
                                     , boardsize       :: !Boardsize
                                     , freeVerticesSet :: !VertexSet
                                     , koBlocked       :: !(Maybe Vertex)
                                     , moveHistory     :: ![Move]
                                     , komi            :: !Score
                                     , blackStones     :: !Int
                                     , whiteStones     :: !Int
                                     , zHash           :: !ZHash
                                     }

showGameState :: GameState -> String
showGameState gState@(GameState goban state) =
    (unlines (zipWith (++) (lines $ showGobanMap goban) $ [
                       ""
                      ,"   blackStones: " ++ show (blackStones state)
                      ,"   whiteStones: " ++ show (whiteStones state)
                      ,"   komi: " ++ show (komi state)
                      ,"   koBlocked: " ++ showKoBlocked (koBlocked state)
                      ,"   score: " ++ show score
                      ,"   zHash: " ++ show (zHash state)
                      ] ++ repeat ""
          -- ++ " moveHistory: " ++ show (moveHistory state)
          -- ++ "\n"
          -- ++ "freeVerticesSet: " ++ show (freeVerticesSet state)
          -- ++ "\n"
         ))
  where
    score = scoreGameState gState
    showKoBlocked (Just p) = gtpShowVertex p
    showKoBlocked Nothing = ""


newGameState :: Boardsize -> Score -> GameState
newGameState n initKomi =
    GameState { getGoban = newGobanMap n
              , getState =
                  GameStateStuff { chains = newChainMap
                                 , boardsize = n
                                 , freeVerticesSet = initFreeVertices
                                 , koBlocked = Nothing
                                 , moveHistory = []
                                 , komi = initKomi
                                 , blackStones = 0
                                 , whiteStones = 0
                                 , zHash = 0
                                 }
              }
  where
    initFreeVertices = S.fromList $ allVertices n



nextMoves :: GameState -> Color -> [Move]
nextMoves (GameState goban state) color =
  Pass color : map Move sanes
  where
    sanes = filter (isSaneMove (GameState goban state)) freeStones
    freeStones = map (flip Stone color) $ freeVertices state


isSaneMove :: GameState -> Stone -> Bool
isSaneMove (GameState goban state) stone =
  -- trace ("isSaneMove called with " ++ show stone) $ do
  if isPotentialFullEye goban stone
  then False
  else
    if isSuicide goban (chains state) stone
    then False
    else True



-- compute game state at the end of a move sequence by replaying it
getLeafGameState :: GameState -> [Move] -> GameState
getLeafGameState = L.foldl' updateGameState

updateGameState :: GameState -> Move -> GameState
updateGameState gState@(GameState goban state) move =
  case move of
    Pass _color ->
      gState { getState = idStuff state move }
    Resign _color ->
      gState { getState = idStuff state move }
    Move stone ->
      gState { getGoban = goban'
             , getState = updateStuff state move dead chains' }
      where
        (goban', chains', dead) = addStone goban (chains state) stone

idStuff :: GameStateStuff -> Move -> GameStateStuff
idStuff state move =
    state { moveHistory = moveHistory state ++ [move]
          , koBlocked = Nothing
          , zHash = zHash'
          }
    where
      zHash' =
          case (koBlocked state) of
            Just p -> updateHash oldZHash (p, EmptyKoBlocked)
            _ -> oldZHash
      oldZHash = zHash state

updateStuff :: GameStateStuff -> Move -> [Stone] -> ChainMap -> GameStateStuff
updateStuff state move@(Move (Stone p c)) dead chains' =
    state { chains = chains'
          , moveHistory = moveHistory state ++ [move]
          , blackStones =
              if c == Black
              then blackStones state + 1
              else blackStones state - length dead
          , whiteStones =
              if c == White
              then whiteStones state + 1
              else whiteStones state - length dead
          , koBlocked =
              case dead of
                [Stone k _] -> Just k
                _ -> Nothing
          , freeVerticesSet =
              S.fromList (map stoneVertex dead)
                   `S.union`
                   S.delete p (freeVerticesSet state)
          , zHash =
              case dead of
                [Stone k _] -> updateHash zHash'' (k, EmptyKoBlocked)
                _ -> zHash''
          }
    where
      zHash'' = foldl f (updateHash zHash' (p, Colored c)) dead
      f h (Stone p' c') = updateHash h (p', Colored c')
      zHash' =
          case (koBlocked state) of
            Just k -> updateHash oldZHash (k, EmptyKoBlocked)
            _ -> oldZHash
      oldZHash = zHash state

            -- str <- showGameStateST state'
            -- trace ("updateGameState" ++ str) $ return ()
updateStuff _ move _ _ = error $ "updateStuff unsupported move: " ++ show move



scoreGameState :: GameState -> Score
scoreGameState (GameState goban state) =
    scoreGameState' state $ map (colorTerritories goban) $ emptyStrings state

scoreGameState' :: GameStateStuff -> [[(Color, [a])]] -> Score
scoreGameState' state colorTs =
    b - w - komi state
    where
      b = fromIntegral $ blackStones state + blackTerritory
      w = fromIntegral $ whiteStones state + whiteTerritory

      blackTerritory = countTerritory Black colorTs
      whiteTerritory = countTerritory White colorTs

      countTerritory color ts =
          sum $ map (fromIntegral . length . snd)
                  $ filter ((== color) . fst) $ concat ts


emptyStrings :: GameStateStuff -> [[Vertex]]
emptyStrings state =
  emptyStrings' initFrees []

    where
      emptyStrings' frees xs 
          | S.null frees = xs
          | otherwise =
              emptyStrings' frees'' (S.toList iMax : xs)
          where
            frees'' = frees' `S.difference` iMax
            iMax = maxSet myAdjacentVertices isFree p
            (p, frees') = S.deleteFindMin frees

      myAdjacentVertices p =
          S.fromList (adjacentVertices p)

      -- maybe actually looking it up in the goban is faster?
      isFree i = i `S.member` initFrees

      initFrees = freeVerticesSet state





centerHeuristic :: Boardsize -> UCTHeuristic Move
centerHeuristic n (Move (Stone (x, y) _color)) =
    -- trace ("centerHeuristic " ++ show (s, (h, (l, m, beta, halfBeta), result)))
    result
    where
      result = (h, 1)

      -- must be between 0 and 1
      h = fromIntegral (minimum [ x, n - x + 1, y, n - y + 1, 3]) ^ (2 :: Int)
          / 9
centerHeuristic _ _ = (0.1, 1)




makeStonesAndLibertyHeuristic :: GameState -> KurtConfig
                              -> UCTHeuristic Move
makeStonesAndLibertyHeuristic (GameState goban state) config =
  stonesAndLibertiesHeu (chains state)

  where
    captureWeight = hCaptureWeight config
    minLibertiesWeight = hMinLibertiesWeight config
    totalLibertiesWeight = hLibertiesWeight config
    chainCountWeight = hChainCountWeight config
    centerWeight = hCenterWeight config

    stonesAndLibertiesHeu :: ChainMap -> UCTHeuristic Move
    stonesAndLibertiesHeu cm (Move stone@(Stone (x, y) _color)) =
        -- trace ("stonesAndLibertiesHeu " ++ show (stone, (h, (stoneH, libertyMinH, libertyAvgH)), result))
        result
        where
          -- must be between 0 and 1
          result = (0.5 + h, 1)

          -- must be between -0.5 and 0.5
          h :: Value
          h = (captureH * fromIntegral captureWeight
               + chainCountH * fromIntegral chainCountWeight
               + minLibertiesH * fromIntegral minLibertiesWeight
               + totalLibertiesH * fromIntegral totalLibertiesWeight
               + centerH * fromIntegral centerWeight)
              / fromIntegral (captureWeight
                              + chainCountWeight
                              + minLibertiesWeight
                              + totalLibertiesWeight
                              + centerWeight)

          -- must be between -0.5 and 0.5
          captureH :: Value
          captureH = min 0.5 $ sqrt (fromIntegral captureC) / fromIntegral (n * 2)
          -- must be between -0.5 and 0.5
          chainCountH :: Value
          chainCountH = 0.5 / (fromIntegral chainC)

          -- stoneH :: Value
          -- stoneH = fromIntegral (signum stoneDiff) * sqrt (fromIntegral $ abs stoneDiff) / fromIntegral (n * 2)
          -- stoneDiff = ourSc - otherSc

          -- must be between -0.5 and 0.5
          minLibertiesH :: Value
          minLibertiesH = (sqrtMin ourMinL - sqrtMin otherMinL) / 2
          sqrtMin m = sqrt $ fromIntegral $ min m 4

          -- must be between -0.5 and 0.5
          totalLibertiesH :: Value
          totalLibertiesH = (fromIntegral ourTotalL - fromIntegral otherTotalL)
                            / fromIntegral (n ^ (2 :: Int))


          -- must be between -0.5 and 0.5
          -- libertyAvgH :: Value
          -- libertyAvgH = (ourLAvg - otherLAvg) / fromIntegral (n ^ (2 :: Int))

          (captureC, chainC, ourMinL, otherMinL, ourTotalL, otherTotalL) =
              stonesAndLiberties goban cm stone


          -- must be between -0.5 and 0.5
          centerH = - halfBeta + beta * (minDist ^ (2 :: Int) / 9)
          minDist = fromIntegral (minimum [ x, n - x + 1, y, n - y + 1, 3])

          -- scaling factor between 1 at the beginning and 0 when l gets big
          beta = fromIntegral n / fromIntegral (l + n)
          halfBeta = beta / 2
          l = length $ moveHistory state


    stonesAndLibertiesHeu _ _ = (0.01, 1)

    n = boardsize state


thisMoveColor :: GameStateStuff -> Color
thisMoveColor state =
    case moveHistory state of
      [] -> trace "thisMoveColor called when moveHistory still empty" White
      moves ->
          case last moves of
            Move (Stone _ color) -> color
            Pass color -> color
            Resign color -> color

nextMoveColor :: GameStateStuff -> Color
nextMoveColor state =
    case moveHistory state of
      [] -> Black
      moves ->
          otherColor $
          case last moves of
            Move (Stone _ color) -> color
            Pass color -> color
            Resign color -> color


freeVertices :: GameStateStuff -> [Vertex]
freeVertices state =
    S.toList $ case koBlocked state of
                 Nothing -> freeVerticesSet state
                 Just p -> S.delete p $ freeVerticesSet state




-- freeVertex :: GameStateST s -> Vertex -> Bool
-- freeVertex state p =
--     i `S.member` (freeVerticesSet state)
--     where
--       i = vertexToInt (boardsize state) p
