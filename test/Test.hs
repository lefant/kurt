{-# OPTIONS -Wall -Werror -Wwarn #-}

module Main (main) where

import qualified Data.HashMap.Strict as H
import qualified Data.Set as S

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import           Data.Goban.GameState
import           Data.Goban.Incremental
import           Data.Goban.Types
import           Data.Goban.ZobristHash

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "kurt"
    [ typesTests
    , gameStateTests
    , incrementalTests
    , zobristTests
    ]

smallBoardMaxSize :: Int
smallBoardMaxSize = 9

zobristBoardSize :: Int
zobristBoardSize = 19

maxRoundTripGtpColumn :: Int
maxRoundTripGtpColumn = 24

typesTests :: TestTree
typesTests = testGroup "Data.Goban.Types"
    [ QC.testProperty "xToLetter / letterToX round-trip parseable x coordinates" prop_xToLetterRoundTrip
    , testCase "GTP coordinates skip I" $ do
        xToLetter 8 @?= 'H'
        xToLetter 9 @?= 'J'
        xToLetter 25 @?= 'Z'
        gtpShowVertex (9, 3) @?= "J3"
    , QC.testProperty "allVertices has n*n unique in-board vertices" prop_allVerticesShape
    , QC.testProperty "borderVertices do not overlap allVertices" prop_borderVerticesDisjoint
    , testCase "adjacent and diagonal vertices are stable" $ do
        adjacentVertices (2, 2) @?= [(2, 1), (1, 2), (3, 2), (2, 3)]
        diagonalVertices (2, 2) @?= [(3, 3), (1, 3), (3, 1), (1, 1)]
    , testCase "move and color helpers are stable" $ do
        otherColor (otherColor Black) @?= Black
        moveColor (Move (Stone (1, 1) Black)) @?= Black
        moveColor (Pass White) @?= White
        moveColor (Resign Black) @?= Black
        isStoneMove (Move (Stone (1, 1) Black)) @?= True
        isStoneMove (Pass Black) @?= False
        isStoneMove (Resign White) @?= False
        gtpShowMove (Move (Stone (1, 1) Black)) @?= "A1"
        gtpShowMove (Pass White) @?= "pass"
        gtpShowMove (Resign Black) @?= "resign"
    ]

prop_xToLetterRoundTrip :: Positive Int -> Property
prop_xToLetterRoundTrip (Positive rawX) =
    let x = 1 + (rawX `mod` maxRoundTripGtpColumn)
    in letterToX (xToLetter x) === x

prop_allVerticesShape :: Positive Int -> Property
prop_allVerticesShape (Positive rawN) =
    let n = smallBoardSize rawN
        vertices = allVertices n
        expected = S.fromList [(x, y) | x <- [1 .. n], y <- [1 .. n]]
    in conjoin [ length vertices === n * n
               , S.fromList vertices === expected
               ]

prop_borderVerticesDisjoint :: Positive Int -> Property
prop_borderVerticesDisjoint (Positive rawN) =
    let n = smallBoardSize rawN
    in S.intersection (S.fromList (allVertices n)) (S.fromList (borderVertices n)) === S.empty

gameStateTests :: TestTree
gameStateTests = testGroup "Data.Goban.GameState"
    [ testCase "new game state has stable initial fields" $ do
        let gs = newGameState 5 6.5
            st = getState gs
        boardsize st @?= 5
        komi st @?= 6.5
        blackStones st @?= 0
        whiteStones st @?= 0
        koBlocked st @?= Nothing
        moveHistory st @?= []
        zHash st @?= 0
        S.size (freeVerticesSet st) @?= 25
        S.fromList (freeVertices st) @?= S.fromList (allVertices 5)
        H.size (getGoban gs) @?= length (borderVertices 5)
    , testCase "initial next move color is black" $ do
        nextMoveColor (getState (newGameState 5 0)) @?= Black
    , testCase "pass and resign preserve board state and free vertices" $ do
        let gs0 = newGameState 5 0
            gs1 = updateGameState gs0 (Pass Black)
            gs2 = updateGameState gs1 (Resign White)
            st0 = getState gs0
            st1 = getState gs1
            st2 = getState gs2
        blackStones st2 @?= 0
        whiteStones st2 @?= 0
        freeVerticesSet st1 @?= freeVerticesSet st0
        freeVerticesSet st2 @?= freeVerticesSet st0
        moveHistory st2 @?= [Pass Black, Resign White]
        koBlocked st2 @?= Nothing
        getGoban gs1 @?= getGoban gs0
        getGoban gs2 @?= getGoban gs0
        thisMoveColor st2 @?= White
        nextMoveColor st2 @?= Black
    , testCase "single stone placement updates observable state" $ do
        let gs = updateGameState (newGameState 5 0) (Move (Stone (1, 1) Black))
            st = getState gs
        blackStones st @?= 1
        whiteStones st @?= 0
        moveHistory st @?= [Move (Stone (1, 1) Black)]
        S.member (1, 1) (freeVerticesSet st) @?= False
        S.size (freeVerticesSet st) @?= 24
        S.fromList (allStones (chains st)) @?= S.singleton (1, 1)
    , testCase "fresh nextMoves include pass and every board vertex" $ do
        let moves = nextMoves (newGameState 3 0) Black
            moveVertices = [vertex | Move (Stone vertex _) <- moves]
        Pass Black `elem` moves @?= True
        S.fromList moveVertices @?= S.fromList (allVertices 3)
        length moveVertices @?= 9
        all ((== Black) . moveColor) moves @?= True
    , testCase "empty board score is negative komi" $ do
        scoreGameState (newGameState 5 6.5) @?= (-6.5)
    ]

incrementalTests :: TestTree
incrementalTests = testGroup "Data.Goban.Incremental"
    [ testCase "center stone has four liberties" $ do
        let (goban, chainMap, dead) = addStone (newGobanMap 5) newChainMap (Stone (3, 3) Black)
            chain = vertexChain goban chainMap (3, 3)
        dead @?= []
        chainColor chain @?= Black
        chainVertices chain @?= S.singleton (3, 3)
        chainLiberties chain @?= S.fromList [(3, 2), (2, 3), (4, 3), (3, 4)]
    , testCase "corner stone has two liberties" $ do
        let (goban, chainMap, _dead) = addStone (newGobanMap 5) newChainMap (Stone (1, 1) Black)
            chain = vertexChain goban chainMap (1, 1)
        S.size (chainLiberties chain) @?= 2
    , testCase "adjacent same-color stones merge into one chain" $ do
        let (goban1, chainMap1, _dead1) = addStone (newGobanMap 5) newChainMap (Stone (3, 3) Black)
            (goban2, chainMap2, _dead2) = addStone goban1 chainMap1 (Stone (3, 4) Black)
            chain1 = vertexChain goban2 chainMap2 (3, 3)
            chain2 = vertexChain goban2 chainMap2 (3, 4)
            expected = S.fromList [(3, 3), (3, 4)]
        chainVertices chain1 @?= expected
        chainVertices chain2 @?= expected
        S.fromList (allStones chainMap2) @?= expected
    ]

zobristTests :: TestTree
zobristTests = testGroup "Data.Goban.ZobristHash"
    [ QC.testProperty "same valid update twice restores original hash" prop_zobristToggle
    , QC.testProperty "distinct valid updates commute" prop_zobristUpdatesCommute
    ]

prop_zobristToggle :: Int -> Positive Int -> Positive Int -> Bool
prop_zobristToggle h (Positive rawX) (Positive rawY) =
    let vertexState = (validHashVertex rawX rawY, Colored Black)
    in updateHash (updateHash h vertexState) vertexState == h

prop_zobristUpdatesCommute :: Int -> Positive Int -> Positive Int -> Bool
prop_zobristUpdatesCommute h (Positive rawX) (Positive rawY) =
    let first = (validHashVertex rawX rawY, Colored Black)
        second = (validHashVertex (rawX + 1) (rawY + 1), Colored White)
    in updateHash (updateHash h first) second == updateHash (updateHash h second) first

smallBoardSize :: Int -> Boardsize
smallBoardSize rawN = 1 + (rawN `mod` smallBoardMaxSize)

validHashVertex :: Int -> Int -> Vertex
validHashVertex rawX rawY =
    (1 + (rawX `mod` zobristBoardSize), 1 + (rawY `mod` zobristBoardSize))
