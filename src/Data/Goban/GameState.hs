{-# OPTIONS -O2 -Wall -Werror -Wwarn #-}
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
                            , GameStateST(..)
                            , GameStateStuff(..)

                            , newGameState
                            , updateGameState
                            , nextMoves
                            , scoreGameState
                            , showGameState
                            , thawGameState
                            , freezeGameStateST

                            , getLeafGameStateST
                            , updateGameStateST
                            , nextMovesST
                            , isSaneMoveST
                            , scoreGameStateST

                            , centerHeuristic
                            , makeStonesAndLibertyHeuristic

                            , freeVertices
                            , thisMoveColor
                            , nextMoveColor
                            ) where


import Control.Monad (filterM, foldM)
import Control.Monad.ST (ST, runST)
-- import Data.Array.IArray (Array)
import Data.Array.MArray (thaw, freeze, unsafeFreeze)
import qualified Data.Set as S

import Kurt.Config
import Data.Goban.Types
import Data.Tree.UCT (UCTHeuristic)
import Data.Tree.UCT.GameTree (Value)
import Data.Goban.Utils
-- import Data.Goban.STVectorGoban (STGoban(..), showGoban, newGoban, copyGoban, addStone, deleteStones)
import Data.Goban.Incremental

import Debug.TraceOrId (trace)



data GameState = GameState { getGoban :: !ChainIdGoban
                           , getState :: !GameStateStuff
                           }

data GameStateST s = GameStateST { getGobanST :: !(ChainIdGobanST s)
                                 , getStateST :: !GameStateStuff
                                 }

data GameStateStuff = GameStateStuff { chains          :: !ChainMap
                                     , boardsize       :: !Boardsize
                                     , freeVerticesSet :: !VertexSet
                                     , koBlocked       :: !(Maybe Vertex)
                                     , moveHistory     :: ![Move]
                                     , komi            :: !Score
                                     , blackStones     :: !Int
                                     , whiteStones     :: !Int
                                     }

showGameState :: GameState -> String
showGameState gState@(GameState goban state) =
    (unlines (zipWith (++) (lines $ showChainIdGoban goban) $ [
                       ""
                      ,"   blackStones: " ++ show (blackStones state)
                      ,"   whiteStones: " ++ show (whiteStones state)
                      ,"   komi: " ++ show (komi state)
                      ,"   koBlocked: " ++ showKoBlocked (koBlocked state)
                      ,"   score: " ++ show score
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
    GameState { getGoban = newChainIdGoban n
              , getState =
                  GameStateStuff { chains = newChainMap
                                 , boardsize = n
                                 , freeVerticesSet = initFreeVertices
                                 , koBlocked = Nothing
                                 , moveHistory = []
                                 , komi = initKomi
                                 , blackStones = 0
                                 , whiteStones = 0
                                 }
              }
  where
    initFreeVertices = S.fromList $ [(x, y) | x <- [1 .. n], y <- [1 .. n]]


freezeGameStateST :: GameStateST s -> ST s GameState
freezeGameStateST (GameStateST gobanST state) = do
  goban <- freeze gobanST
  return $ GameState goban state

thawGameState :: GameState -> ST s (GameStateST s)
thawGameState (GameState goban state) = do
  gobanST <- thaw goban
  return $ GameStateST gobanST state



nextMoves :: GameState -> Color -> [Move]
nextMoves (GameState goban state) color =
  Pass color : map Move sanes
  where
    sanes =
        runST $ do
          gobanST <- thaw goban
          filterM (isSaneMoveST (GameStateST gobanST state)) freeStones

    freeStones = map (flip Stone color) $ freeVertices state

nextMovesST :: GameStateST s -> Color -> ST s [Move]
nextMovesST (GameStateST gobanST state) color = do
  sanes <- filterM (isSaneMoveST (GameStateST gobanST state)) freeStones
  return $ Pass color : map Move sanes
  where
    freeStones = map (flip Stone color) $ freeVertices state



isSaneMoveST :: GameStateST s -> Stone -> ST s Bool
isSaneMoveST (GameStateST goban state) stone = do
  -- trace ("isSaneMove called with " ++ show stone) $ do
  potEye <- isPotentialFullEye goban stone
  (if potEye
   then
       -- trace ("isSaneMove potEye " ++ show stone) $
       return False
   else do
     -- suicide <- isSuicideVertex g color p
     suicide <- isSuicide goban (chains state) stone
     (if suicide
      then
          -- trace ("isSaneMove suicide" ++ show stone) $
          return False
      else return True))



-- compute game state at the end of a move sequence by replaying it
getLeafGameStateST :: GameState -> [Move] -> ST s (GameStateST s)
getLeafGameStateST (GameState goban state) moves = do
  gobanST <- thaw goban
  let gState = GameStateST { getGobanST = gobanST
                           , getStateST = state }
  foldM updateGameStateST gState moves



updateGameState :: GameState -> Move -> GameState
updateGameState gState@(GameState goban state) move =
  case move of
    Pass _color ->
        gState { getState = idStuff state move }
    Resign _color ->
        gState { getState = idStuff state move }
    Move stone ->
        runST $
        do
          gobanST <- thaw goban
          (chains', dead) <- addChainStone gobanST (chains state) stone
          -- gobanST goes out of scope after this function, so unsafe is ok here
          goban' <- unsafeFreeze gobanST
          return $ gState { getGoban = goban'
                          , getState = updateStuff state move dead chains' }


updateGameStateST :: GameStateST s -> Move -> ST s (GameStateST s)
updateGameStateST gState@(GameStateST goban state) move = do
  case move of
    Pass _color ->
        return $ gState { getStateST = idStuff state move }
    Resign _color ->
        return $ gState { getStateST = idStuff state move }
    Move stone ->
        do
          (chains', dead) <- addChainStone goban (chains state) stone
          return $ gState { getGobanST = goban
                          , getStateST = updateStuff state move dead chains' }

idStuff :: GameStateStuff -> Move -> GameStateStuff
idStuff state move =
    state { moveHistory = moveHistory state ++ [move]
          , koBlocked = Nothing }
    

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
          }

            -- str <- showGameStateST state'
            -- trace ("updateGameState" ++ str) $ return ()
updateStuff _ move _ _ = error $ "updateStuff unsupported move: " ++ show move



scoreGameStateST :: GameStateST s -> ST s Score
scoreGameStateST (GameStateST gobanST state) = do
  colorTs <- mapM (colorTerritories gobanST) $ emptyStrings state
  return $ scoreGameState' state colorTs

scoreGameState :: GameState -> Score
scoreGameState (GameState goban state) =
    scoreGameState' state colorTs
    where
      colorTs =
          runST $ do
            gobanST <- thaw goban
            mapM (colorTerritories gobanST) $ emptyStrings state

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
