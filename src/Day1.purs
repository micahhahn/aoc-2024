module Day1
  ( challenge1
  , challenge2
  ) where

import Prelude

import Challenge (Challenge)
import Data.Array (foldl, sort, unzip, zip)
import Data.Foldable (sum)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Parser (Parser, crashLeft, many1, newline, number, runParser, space, sepByArray)

challenge1 :: Challenge
challenge1 =
  { name: "Day 1 Part 1"
  , examplePrompt:
      [ "3   4"
      , "4   3"
      , "2   5"
      , "1   3"
      , "3   9"
      , "3   3"
      ]
  , exampleAnswer: "11"
  , solver: solution1
  , promptPath: "assets/day1.txt"
  , solution: Just "2000468"
  }

solution1 :: String -> String
solution1 input =
  let
    result = parse input
    (l /\ r) = unzip result
    sorted = zip (sort l) (sort r)
  in
    sorted
      # map (\(l /\ r) -> abs (l - r))
      # sum
      # show

parseLine :: Parser String (Tuple Int Int)
parseLine = do
  left <- number
  _ <- many1 space
  right <- number
  pure (left /\ right)

parse :: String -> Array (Tuple Int Int)
parse input =
  runParser input (parseLine `sepByArray` newline)
    # crashLeft

abs :: Int -> Int
abs i =
  if i < 0 then
    -i
  else
    i

challenge2 :: Challenge
challenge2 =
  { name: "Day 1 Part 2"
  , examplePrompt: challenge1.examplePrompt
  , exampleAnswer: "31"
  , solver: solution2
  , promptPath: "assets/day1.txt"
  , solution: Just "18567089"
  }

solution2 :: String -> String
solution2 input =
  let
    result = parse input
    (l /\ r) = unzip result
    m = Map.fromFoldableWith (+) (map (\x -> x /\ 1) r)
  in
    foldl
      ( \a x ->
          Map.lookup x m
            # Maybe.fromMaybe 0
            # (*) x
            # (+) a
      )
      0
      l
      # show