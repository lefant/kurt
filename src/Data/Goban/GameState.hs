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
                            , newGameState
                            , showGameState
                            , scoreGameState
                            , getLeafGameState
                            , updateGameState
                            , centerHeuristic
                            , makeStonesAndLibertyHeuristic
                            , nextMoves
                            , isSaneMove
                            , freeVertices
                            , thisMoveColor
                            , nextMoveColor
                            ) where


import Control.Monad (filterM, foldM)
import Control.Monad.ST (ST)
-- import Data.Array.IArray (Array)
import Data.Array.MArray (freeze, thaw)
import qualified Data.Set as S

import Kurt.Config
import Data.Goban.Types
import Data.Tree.UCT (UCTHeuristic)
import Data.Tree.UCT.GameTree (Value)
import Data.Goban.Utils
import Data.Goban.STVectorGoban
import Data.Goban.Incremental

import Debug.TraceOrId (trace)



data GameState s = GameState { goban           :: !(STGoban s)
                             , chainGoban      :: !(ChainIdGoban s)
                             , chains          :: !ChainMap
                             , boardsize       :: !Boardsize
                             , freeVerticesSet :: !VertexSet
                             , koBlocked       :: !(Maybe Vertex)
                             , moveHistory     :: ![Move]
                             , komi            :: !Score
                             , blackStones     :: !Int
                             , whiteStones     :: !Int
                             -- , blackPrisoners  :: !Score
                             -- , whitePrisoners  :: !Score
                             }

showGameState :: GameState s -> ST s String
showGameState state = do
  gobanStr <- showGoban $ goban state
  score <- scoreGameState state
  -- chainGobanStr <- showChainIdGoban $ chainGoban state
  -- return $ chainGobanStr ++ "\n"
  --            ++
  return $ (unlines (zipWith (++) (lines gobanStr) $ [
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
    showKoBlocked (Just p) = gtpShowVertex p
    showKoBlocked Nothing = ""

newGameState :: Boardsize -> Score -> ST s (GameState s)
newGameState n initKomi = do
  g <- newGoban n
  cg <- newChainGoban n
  return GameState { goban = g
                   , chainGoban = cg
                   , chains = newChainMap
                   , boardsize = n
                   , freeVerticesSet = initFreeVertices
                   , koBlocked = Nothing
                   , moveHistory = []
                   , komi = initKomi
                   , blackStones = 0
                   , whiteStones = 0
                   -- , blackPrisoners = 0
                   -- , whitePrisoners = 0
                   }
  where
    initFreeVertices = S.fromList $ [(x, y) | x <- [1 .. n], y <- [1 .. n]]




nextMoves :: GameState s -> Color -> ST s [Move]
nextMoves state color = do
  let freeStones = map (flip Stone color) $ freeVertices state
  sanes <- filterM (isSaneMove state) freeStones
  return $ Pass color : map Move sanes


isSaneMove :: GameState s -> Stone -> ST s Bool
isSaneMove state stone = do
  -- trace ("isSaneMove called with " ++ show stone) $ do
  potEye <- isPotentialFullEye (goban state) stone
  (if potEye
   then
       -- trace ("isSaneMove potEye " ++ show stone) $
       return False
   else do
     -- suicide <- isSuicideVertex g color p
     suicide <- isSuicide (chainGoban state) (chains state) stone
     (if suicide
      then
          -- trace ("isSaneMove suicide" ++ show stone) $
          return False
      else return True))



-- compute game state at the end of a move sequence by replaying it
getLeafGameState :: GameState s -> [Move] -> ST s (GameState s)
getLeafGameState state moves = do
  g' <- copyGoban $ goban state
  fcg :: ChainIdGobanFrozen <- (freeze $ chainGoban state)
  -- fcg :: (Array Vertex Int) <- (freeze $ chainGoban state)
  cg' <- thaw fcg
  let state' = state { goban = g', chainGoban = cg' }
  foldM updateGameState state' moves



updateGameState :: GameState s -> Move -> ST s (GameState s)
updateGameState state move =
    case move of
      Pass _color ->
          return $ state {
                       moveHistory = moveHistory state ++ [move]
                     , koBlocked = Nothing
                     }
      Resign _color ->
          return $ state {
                       moveHistory = moveHistory state ++ [move]
                     , koBlocked = Nothing
                     }
      Move stone@(Stone p c) ->
          do
            -- incremental
            (chains', dead) <- addChainStone cg (chains state) stone

            addStone g stone
            deleteStones g dead

            let state' = state {
                        goban = g
                      , chainGoban = cg
                      , chains = chains'
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
                      , freeVerticesSet = freeVerticesSet' p dead
                      }

            -- str <- showGameState state'
            -- trace ("updateGameState" ++ str) $ return ()

            return state'
    where
      g = goban state
      cg = chainGoban state

      freeVerticesSet' p dead =
          S.fromList (map stoneVertex dead)
          `S.union`
          S.delete p (freeVerticesSet state)


scoreGameState :: GameState s -> ST s Score
scoreGameState state = do
  let empties = emptyStrings state
  colorTs <- mapM (colorTerritories (goban state)) empties
  let blackTerritory = countTerritory Black colorTs
  let whiteTerritory = countTerritory White colorTs
  let b = fromIntegral $ blackStones state + blackTerritory
  let w = fromIntegral $ whiteStones state + whiteTerritory
  -- trace ("scoreGameState empties " ++ show empties) $ return ()
  -- trace ("scoreGameState colorTs " ++ show colorTs) $ return ()
  -- trace ("scoreGameState stones " ++ show (blackStones state, whiteStones state)) $ return ()
  -- trace ("scoreGameState " ++ show ((blackTerritory, b), (whiteTerritory, w))) $ return ()
  return $ b - w - komi state

  where
    countTerritory color ts =
        sum $ map (fromIntegral . length . snd)
                $ filter ((== color) . fst) $ concat ts


emptyStrings :: GameState s -> [[Vertex]]
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




makeStonesAndLibertyHeuristic :: GameState s -> KurtConfig
                              -> ST s (UCTHeuristic Move)
makeStonesAndLibertyHeuristic state config = do
  fcg :: ChainIdGobanFrozen <- (freeze $ chainGoban state)
  return $ stonesAndLibertiesHeu fcg (chains state)

  where
    captureWeight = hCaptureWeight config
    minLibertiesWeight = hMinLibertiesWeight config
    totalLibertiesWeight = hLibertiesWeight config
    chainCountWeight = hChainCountWeight config
    centerWeight = hCenterWeight config

    stonesAndLibertiesHeu :: ChainIdGobanFrozen -> ChainMap
                          -> UCTHeuristic Move
    stonesAndLibertiesHeu cg cm (Move stone@(Stone (x, y) _color)) =
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
              stonesAndLiberties cg cm stone


          -- must be between -0.5 and 0.5
          centerH = - halfBeta + beta * (minDist ^ (2 :: Int) / 9)
          minDist = fromIntegral (minimum [ x, n - x + 1, y, n - y + 1, 3])

          -- scaling factor between 1 at the beginning and 0 when l gets big
          beta = fromIntegral n / fromIntegral (l + n)
          halfBeta = beta / 2
          l = length $ moveHistory state


    stonesAndLibertiesHeu _ _ _ = (0.01, 1)

    n = boardsize state


thisMoveColor :: GameState s -> Color
thisMoveColor state =
    case moveHistory state of
      [] -> trace "thisMoveColor called when moveHistory still empty" White
      moves ->
          case last moves of
            Move (Stone _ color) -> color
            Pass color -> color
            Resign color -> color

nextMoveColor :: GameState s -> Color
nextMoveColor state =
    case moveHistory state of
      [] -> Black
      moves ->
          otherColor $
          case last moves of
            Move (Stone _ color) -> color
            Pass color -> color
            Resign color -> color


freeVertices :: GameState s -> [Vertex]
freeVertices state =
    S.toList $ case koBlocked state of
                 Nothing -> freeVerticesSet state
                 Just p -> S.delete p $ freeVerticesSet state




-- freeVertex :: GameState s -> Vertex -> Bool
-- freeVertex state p =
--     i `S.member` (freeVerticesSet state)
--     where
--       i = vertexToInt (boardsize state) p
