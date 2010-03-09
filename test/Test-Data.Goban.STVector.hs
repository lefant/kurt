{-# OPTIONS_GHC -O2 -Wall -Werror -Wwarn #-}

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck (testProperty)

import Test.QuickCheck

import Data.Goban.Goban
import Data.Goban.STVector


-- instance Arbitrary VertexState where
--     arbitrary     = choose (chr 0, chr 255)
--     coarbitrary c = variant (ord c `rem` 4)


main :: IO ()
main = do
  defaultMain tests

tests =
    [ testGroup "Data.Goban.STVector tests"
      [ testProperty "intToVertex . vertexToInt == id" prop_vertexToIntToVertex
      , testProperty "vertexToInt . intToVertex == id" prop_intToVertexToInt
      ]
    ]


prop_vertexToIntToVertex :: Boardsize -> Int -> Int -> Property
prop_vertexToIntToVertex n x y =
    n > 0 && x >= 0 && y >= 0 && x <= n + 1 && y <= n + 1 ==>
    vertexToIntToVertex n (x, y) == (x, y)

vertexToIntToVertex :: Boardsize -> Vertex -> Vertex
vertexToIntToVertex n (x,y) = intToVertex n (vertexToInt n (x, y))

prop_intToVertexToInt :: Boardsize -> Int -> Property
prop_intToVertexToInt n i =
    n > 0 && i >= 0 && i <= maxIntIndex n ==>
    vertexToInt n (intToVertex n i) == i
