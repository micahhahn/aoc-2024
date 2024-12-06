module Day4
  ( challenge1
  , challenge2
  ) where

import Prelude

import Challenge (Challenge)
import Data.List (concatMap, catMaybes, List(..), (:))
import Data.List as List
import Data.List.Lazy as Lazy
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Matrix (Matrix)
import Matrix as Matrix
import Parser (Parser, manyArray, runParser, crashLeft, sepByArray, newline, notNewline)

challenge1 :: Challenge
challenge1 =
  { name: "Day 4 Part 1"
  , examplePrompt:
      [ "MMMSXXMASM"
      , "MSAMXMSMSA"
      , "AMXSXMAAMM"
      , "MSAMASMSMX"
      , "XMASAMXAMM"
      , "XXAMMXXAMA"
      , "SMSMSASXSS"
      , "SAXAMASAAA"
      , "MAMMMXMMMM"
      , "MXMXAXMASX"
      ]
  , exampleAnswer: "18"
  , solver: solution1
  , promptPath: "assets/day4.txt"
  , solution: Just "2646"
  }

challenge2 :: Challenge
challenge2 =
  { name: "Day 4 Part 2"
  , examplePrompt: challenge1.examplePrompt
  , exampleAnswer: "9"
  , solver: solution2
  , promptPath: "assets/day4.txt"
  , solution: Just "2000"
  }

parseGrid :: Parser String (Array (Array Char))
parseGrid = manyArray notNewline `sepByArray` newline

parse :: String -> Matrix Char
parse input =
  runParser input parseGrid
    # crashLeft
    # Matrix.fromArray
    # Maybe.fromMaybe Matrix.empty

target :: Lazy.List Char
target = Lazy.fromFoldable [ 'X', 'M', 'A', 'S' ]

checkSolution :: Matrix Char -> Tuple Int Int -> List (Tuple Char (Tuple Int Int)) -> Maybe (Tuple Int Int)
checkSolution matrix =
  List.foldM
    ( \(x /\ y) (char /\ (dX /\ dY)) -> do
        matrixChar <- Matrix.get x y matrix
        if matrixChar == char then Just ((x + dX) /\ (y + dY)) else Nothing
    )

solution1 :: String -> String
solution1 input =
  let
    matrix = parse input
    solutions = do
      x <- List.range 0 (Matrix.width matrix - 1)
      y <- List.range 0 (Matrix.height matrix - 1)
      dX <- List.range (-1) 1
      dY <- List.range (-1) 1
      let steps = List.fromFoldable $ Lazy.zip target (Lazy.repeat (dX /\ dY))
      pure $ checkSolution matrix (x /\ y) steps

  in
    solutions
      # catMaybes
      # List.length
      # show

masTarget :: Lazy.List Char
masTarget = Lazy.cycle (Lazy.fromFoldable [ 'M', 'S', 'S', 'M' ])

solution2 :: String -> String
solution2 input =
  let
    matrix = parse input
    vector = Lazy.fromFoldable [ 2 /\ 0, 0 /\ 2, (-2) /\ 0, 0 /\ (-2) ]
    solutions = do
      x <- List.range 1 (Matrix.width matrix - 2)
      y <- List.range 1 (Matrix.height matrix - 2)
      i <- List.range 0 3
      let baseSteps = Lazy.zip (Lazy.drop i masTarget) vector
      let steps = List.fromFoldable $ Lazy.cons ('A' /\ ((-1) /\ (-1))) baseSteps
      pure $ checkSolution matrix (x /\ y) steps
  in
    solutions
      # catMaybes
      # List.length
      # show

