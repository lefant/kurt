{-# OPTIONS -O2 -Wall -Werror -Wwarn #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

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
                            , scoreGameState
                            -- , getLeafGameState
                            -- , updateGameState
                            , nextMoves
                            , thisMoveColor
                            , nextMoveColor
                            ) where


import Control.Monad (filterM)
import Control.Monad.ST (ST)
import Data.List (partition, foldl', (\\))
import qualified Data.IntSet as S


import Data.Goban.Goban
import Data.Goban.Utils
import Data.Goban.STVector (STGoban)
import Data.Goban.STVector (newGoban, size, intToVertex, gobanSize, borderVertices, intAscAdjacentVertices, adjacentStones, intAdjacentStones, isSuicideVertex)
import Data.Goban.STVector (addStone, deleteStones)


type VertexSet = S.IntSet

data GameState s = GameState { goban           :: !(STGoban s)
                             , boardsize       :: !Boardsize
                             , freeVertices    :: !VertexSet
                             , koBlocked       :: !VertexSet
                             , moveHistory     :: ![Move]
                             , komi            :: !Score
                             , blackStones     :: !Int
                             , whiteStones     :: !Int
                             -- , blackPrisoners  :: !Score
                             -- , whitePrisoners  :: !Score
                             }


-- instance Show GameState where
--     show state =
--         case moveHistory state of
--           [] -> ""
--           moves ->
--               show $ last moves
--               -- case last moves of
--               --   (StoneMove (Stone ((x, y), color))) ->
--               --       c ++ [(xToLetter x)] ++ (show y)
--               --       where
--               --         c = case color of
--               --               Black -> "b "
--               --               White -> "w "
--               --   (Pass _color) -> "pass"
--               --   (Resign _color) -> "resign"


newGameState :: Boardsize -> Score -> ST s (GameState s)
newGameState n initKomi = do
  g <- newGoban n
  return GameState { goban = g
                   , boardsize = n
                   , freeVertices = initFreeVertices
                   , koBlocked = S.empty
                   , moveHistory = []
                   , komi = initKomi
                   , blackStones = 0
                   , whiteStones = 0
                   -- , blackPrisoners = 0
                   -- , whitePrisoners = 0
                   }
  where
    initFreeVertices = S.fromList $ [0 .. (size n)] \\ (borderVertices n)





-- compute game state at the end of a move sequence by replaying it
-- getLeafGameState :: GameState -> [Move] -> GameState
-- getLeafGameState = foldl' updateGameState





-- -- STUArray s Coord Char) -> Coord -> Move -> ST s Bool
-- updateGameState :: GameState -> Move -> GameState
-- updateGameState state move =
--     case move of
--       StoneMove stone@(Stone (p, _)) ->
--           if p `elem` (koBlocked state)
--           then error "updateGameState: move in ko violation"
--           else
--                   -- trace ("updateGameState: "
--                   --        ++ show (
--                   --                 (" move ", move)
--                   --                 ,(" bp: ", blackPrisoners')
--                   --                 ,(" wp: ", whitePrisoners')
--                   --                 ,(" dead: ", dead)
--                   --                 ,(" dead': ", dead')
--                   --                 ,(" bdead': ", bDead)
--                   --                 ,(" wdead': ", wDead)
--                   --                ))
--               state {
--                        goban = goban''
--                       ,moveHistory = (moveHistory state) ++ [move]
--                       ,blackPrisoners = blackPrisoners'
--                       ,whitePrisoners = whitePrisoners'
--                       ,koBlocked = koBlocked'
--               }
--           where
--             dead = killedStones goban' stone
--             goban' = addStone (goban state) stone
--             goban'' = deleteStones goban' dead
--             -- goban''' = deleteStones goban'' dead'
--             -- dead' =
--             --     if isDead goban'' stone
--             --     then (groupOfStone goban'' stone)
--             --     else []
--             blackPrisoners' =
--                 (blackPrisoners state)
--                 + (fromIntegral $ length bDead)
--             whitePrisoners' =
--                 (whitePrisoners state)
--                 + (fromIntegral $ length wDead)
--             (bDead, wDead) = partition
--                              (\(Stone (_, c)) -> c == Black)
--                              -- (dead ++ dead')
--                              dead

--             koBlocked' =
--                 case dead of
--                   [koStone@(Stone (v,_))] ->
--                       if [stone] == (killedStones goban' koStone)
--                       then [v]
--                       else []
--                   _ -> []


--       Pass _color ->
--           state {
--                 moveHistory = (moveHistory state) ++ [move]
--                ,koBlocked = []
--               }

--       Resign _color ->
--           state {
--                 moveHistory = (moveHistory state) ++ [move]
--                ,koBlocked = []
--               }


-- updateGameState :: GameState -> Move -> GameState
-- updateGameState state move =
--     case move of
--       StoneMove stone@(Stone (p, _)) ->
--           if p `elem` (koBlocked state)
--           then error "updateGameState: move in ko violation"
--           else
--                   -- trace ("updateGameState: "
--                   --        ++ show (
--                   --                 (" move ", move)
--                   --                 ,(" bp: ", blackPrisoners')
--                   --                 ,(" wp: ", whitePrisoners')
--                   --                 ,(" dead: ", dead)
--                   --                 ,(" dead': ", dead')
--                   --                 ,(" bdead': ", bDead)
--                   --                 ,(" wdead': ", wDead)
--                   --                ))
--               state {
--                        goban = goban''
--                       ,moveHistory = (moveHistory state) ++ [move]
--                       ,blackPrisoners = blackPrisoners'
--                       ,whitePrisoners = whitePrisoners'
--                       ,koBlocked = koBlocked'
--               }
--           where
--             dead = killedStones goban' stone
--             goban' = addStone (goban state) stone
--             goban'' = deleteStones goban' dead
--             -- goban''' = deleteStones goban'' dead'
--             -- dead' =
--             --     if isDead goban'' stone
--             --     then (groupOfStone goban'' stone)
--             --     else []
--             blackPrisoners' =
--                 (blackPrisoners state)
--                 + (fromIntegral $ length bDead)
--             whitePrisoners' =
--                 (whitePrisoners state)
--                 + (fromIntegral $ length wDead)
--             (bDead, wDead) = partition
--                              (\(Stone (_, c)) -> c == Black)
--                              -- (dead ++ dead')
--                              dead

--             koBlocked' =
--                 case dead of
--                   [koStone@(Stone (v,_))] ->
--                       if [stone] == (killedStones goban' koStone)
--                       then [v]
--                       else []
--                   _ -> []


--       Pass _color ->
--           state {
--                 moveHistory = (moveHistory state) ++ [move]
--                ,koBlocked = []
--               }

--       Resign _color ->
--           state {
--                 moveHistory = (moveHistory state) ++ [move]
--                ,koBlocked = []
--               }



scoreGameState :: GameState s -> ST s Score
scoreGameState state = do
  empties <- return $ emptyStrings state
  colorTs <- mapM (colorTerritories (goban state)) empties
  blackTerritory <- return $ countTerritory Black colorTs
  whiteTerritory <- return $ countTerritory White colorTs
  b <- return $ fromIntegral $ (blackStones state) + blackTerritory
  w <- return $ fromIntegral $ (whiteStones state) + whiteTerritory
  return $ b - w - (komi state)

  where
    countTerritory color ts =
        sum $ map (fromIntegral . length . snd)
                $ filter ((== color) . fst) $ concat ts

colorTerritories :: STGoban s -> [Int] -> ST s ([(Color, [Int])])
colorTerritories g t = do
  maybeColor <- intAllAdjacentStonesSameColor g t
  return $ case maybeColor of
             Just tColor -> [(tColor, t)]
             Nothing -> []

intAllAdjacentStonesSameColor :: STGoban s -> [Int] -> ST s (Maybe Color)
intAllAdjacentStonesSameColor g ps = do
  as <- mapM (intAdjacentStones g) ps
  return $ maybeSameColor $ map stoneColor $ concat as
    where
      maybeSameColor [] = Nothing
      maybeSameColor (c : cs) =
          if all (c ==) cs
          then Just c
          else Nothing


emptyStrings :: GameState s -> [[Int]]
emptyStrings state =
    emptyStrings' initFrees []

    where
      emptyStrings' frees xs 
          | S.null frees = xs
          | otherwise =
              emptyStrings' frees'' ((S.toList iMax) : xs)
          where
            frees'' = frees' `S.intersection` iMax
            iMax = maxIntSet adjacentVertices isFree i
            (i, frees') = S.deleteFindMin frees

      adjacentVertices i =
          S.fromDistinctAscList (intAscAdjacentVertices n i)

      -- maybe actually looking it up in the goban is faster?
      isFree i = i `S.member` initFrees

      initFrees = freeVertices state

      n = boardsize state


maxIntSet :: (Int -> S.IntSet) -> (Int -> Bool) -> Int -> S.IntSet
maxIntSet genF filterF p =
    maxIntSet' (S.singleton p) S.empty
    where
      maxIntSet' is js
          | S.null is = js
          | otherwise = maxIntSet' is'' js'
          where
            is'' = S.union is' $ S.difference ks js
            js' = S.insert i js
            ks = S.filter filterF $ genF i
            (i, is') = S.deleteFindMin is



nextMoves :: GameState s -> Color -> ST s [Move]
nextMoves state color =
    filterM (isSaneMove state color) frees >>=
            (return . (map (\v -> StoneMove (Stone (v, color)))))
    where
      frees =
          map (intToVertex (boardsize state))
                  $ S.toList
                  $ S.difference (freeVertices state) (koBlocked state)

--       -- frees =
--       --     if length (moveHistory state) > m
--       --     then
--       --         freeVertices goban
--       --     else
--       --         freeNonEdgeVertices goban

--       -- m = truncate $ sqrt (fromIntegral (sizeOfGoban goban) :: Float)


isSaneMove :: GameState s -> Color -> Vertex -> ST s Bool
isSaneMove state color p =
    -- (not (isPotentialFullEye g color p)) &&
    isSuicideVertex g color p >>= (return . not)
    where
      g = goban state

thisMoveColor :: GameState s -> ST s Color
thisMoveColor state =
    return $ case moveHistory state of
               [] ->
                   error "thisMoveColor called when moveHistory still empty"
               moves ->
                   case last moves of
                     (StoneMove (Stone (_, color))) -> color
                     (Pass color) -> color
                     (Resign color) -> color

nextMoveColor :: GameState s -> ST s Color
nextMoveColor state =
    return $ case moveHistory state of
               [] -> Black
               moves ->
                   otherColor $
                   case last moves of
                     (StoneMove (Stone (_, color))) -> color
                     (Pass color) -> color
                     (Resign color) -> color

