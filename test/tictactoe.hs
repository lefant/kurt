{-# OPTIONS -O2 -Wall -Werror -Wwarn #-}

import Data.List (unfoldr, foldl1', intersect, group, partition, (\\), sort)
import System.Random (newStdGen)
-- import Control.Monad.Random (getRandomR)
-- import Data.Tree (drawTree)
-- import Debug.Trace (trace)

import Data.Tree.UCT


type Coord = (Int, Int)


data Value = Cross | Naught | Empty
           deriving (Eq, Ord)

instance Show Value where
    show Cross = "X"
    show Naught = "O"
    show Empty = "."

instance Read Value where
    readsPrec _ [value] =
        [(v, "")]
        where
          v = case value of
                'X' -> Cross
                'O' -> Naught
                '.' -> Empty
                other -> error ("Read Value: unknown: " ++ show other)
    readsPrec _ [] = error "Read Value: empty string is not a Value"
    readsPrec _ ls = error ("Read Value: only single Char is a Value: "
                            ++ show ls)


type Pair = (Coord, Value)


data Board = Board [Pair]

instance Show Board where
    show (Board ls) =
        "\n" ++ (show' $ sort ls)
        where
          show' [] = ""
          show' ls' = concatMap (show . snd) left ++ "\n" ++ show' right
          -- show' ls' = concatMap (show . snd) left ++ show' right
              where
                (left, right) = splitAt n ls'

instance Read Board where
    -- readsPrec is the main function for parsing input
    readsPrec _ value =
        if length b == n2
        then [(Board b, "")]
        else error ("Read Board: wrong number of elements: "
                 ++ show b)

        where
          b = zip pairs $ concat values
          values = unfoldr f value

          f :: String -> Maybe ([Value], String)
          f str =
              if null left
              then Nothing
              else Just (map (read . (:[])) left', right)
              where
                (left, right) = splitAt n str
                left' = filter (\c -> c `elem` "XO.") left


newtype Valid = Valid Bool

instance Show Valid where
    show (Valid a) = case a of
      True -> "valid"
      False -> "invalid"



instance UctNode Board where
    isTerminalNode = isValidEndBoard

    finalResult board =
        case unicolorLines board of
          [] -> 0.5
          [l] -> computeResult $ valueOfLine l
          ls ->
              case intersectLines ls of
                [] -> error "finalResult: invalid board position"
                [p] -> computeResult $ valueOfPair p
                _otherwise -> error "finalResult: invalid board position"
        where
          computeResult winner =
              -- trace ("computeResult: " ++ show (result, board))
              result
              where
                result =
                    if toMove board == winner
                    then 0.0
                    else 1.0

    -- randomEvalOnce _board = getRandomR (0, 1)
    randomEvalOnce _board = return 0.9

    children board@(Board ps) =
        map Board $ [[(play x mover)] ++ (e \\ [x]) ++ f | x <- e ]
        where
          (e, f) = partition ((Empty ==) . snd) ps
          mover = toMove board


n :: Int
n = 3
n2 :: Int
n2 = n ^ (2 :: Int)

pairs :: [Coord]
pairs = [(a, b) | b <- [1 .. n], a <- [1 .. n]]



main :: IO ()
main = do
  runs <- getLine
  rGen <- newStdGen
  result <- return $ uct emptyBoard (read runs) rGen
  print result

emptyBoard :: Board
emptyBoard = Board $ zip pairs $ repeat Empty

toMove :: Board -> Value
toMove board =
    if crossCount == naughtCount
    then Cross
    else if crossCount == naughtCount + 1
         then Naught
         else error "toMove: invalid board position"
    where
      crossCount = countMoves Cross board
      naughtCount = countMoves Naught board

play :: Pair -> Value -> Pair
play (c, Empty) value = (c, value)
play (_, _) _ = error "play: illegal move"


-- runAll :: String -> String
-- runAll str =
--     unlines $ map (show . Valid . isValidEndBoard) boards
--     where
--       boards = map read
--                $ filter (/= "")
--                $ map (filter (\c -> c `elem` "XO."))
--                $ lines str


isValidEndBoard :: Board -> Bool
isValidEndBoard board =
    case unicolorLines board of
      [] -> totalCount == n2 && crossCount == naughtCount + 1
      [l] -> lastBy $ valueOfLine l
      ls ->
          case intersectLines ls of
            [] -> False
            [p] -> lastBy $ valueOfPair p
            _otherwise -> False
    where
      lastBy :: Value -> Bool
      lastBy Cross = crossCount == naughtCount + 1
      lastBy Naught = crossCount == naughtCount
      lastBy _ = False

      crossCount = countMoves Cross board
      naughtCount = countMoves Naught board
      totalCount = crossCount + naughtCount


valueOfLine :: [Pair] -> Value
valueOfLine = valueOfPair . head

valueOfPair :: Pair -> Value
valueOfPair = snd

countMoves :: Value -> Board -> Int
countMoves value (Board board) = length $ filter ((== value) . snd) board

intersectLines :: [[Pair]] -> [Pair]
intersectLines = (foldl1' intersect)

unicolorLines :: Board -> [[Pair]]
unicolorLines (Board board) = filter unicolorLine $ lineCands board

unicolorLine :: [Pair] -> Bool
unicolorLine ps =
    (length grouped == 1) && not (Empty == (head $ head grouped))
    where
      grouped = (group . map snd) ps

lineCands :: [Pair] -> [[Pair]]
lineCands ps =
    horizontals ++ verticals ++ diagonals
    where
      horizontals :: [[Pair]]
      horizontals = map (flip filter ps . yEqs) [1 .. n]
          where
            yEqs = flip (((==)) . (snd . fst))
      verticals :: [[Pair]]
      verticals = map (flip filter ps . xEqs) [1 .. n]
          where
            xEqs = flip (((==)) . (fst . fst))
      diagonals :: [[Pair]]
      diagonals =
          [d1, d2]
          where
            d1 = filter (\(x, _) -> fst x == snd x) ps
            d2 = filter (\(x, _) -> (fst x) + (snd x) == (n + 1)) ps
