module Day2
  ( challenge1
  ) where

import Prelude

import Challenge (Challenge)
import Data.Array (zip)
import Data.Array as Array
import Data.Foldable (sum)
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Ord (abs)
import Data.Tuple.Nested ((/\))
import Parser (Parser, number, sepByArray, space, runParser, newline, crashLeft)

challenge1 :: Challenge
challenge1 =
  { name: "Day 2 Part 1"
  , examplePrompt:
      [ "7 6 4 2 1"
      , "1 2 7 8 9"
      , "9 7 6 2 1"
      , "1 3 2 4 5"
      , "8 6 4 4 1"
      , "1 3 6 7 9"
      ]
  , exampleAnswer: "2"
  , solver: solution1
  , promptPath: "assets/day2.txt"
  , solution: Just "269"
  }

newtype Report = Report (Array Int)

instance Show Report where
  show (Report array) = "Report " <> show array

parseReport :: Parser String Report
parseReport = sepByArray number space <#> Report

parse :: String -> Array Report
parse input =
  runParser input (parseReport `sepByArray` newline)
    # crashLeft

solution1 :: String -> String
solution1 input =
  parse input
    # map (\x -> if isSafeN 0 x then 1 else 0)
    # sum
    # show

isSafeN :: Int -> Report -> Boolean
isSafeN dropable (Report levels) =
  let
    go n todoPairs cmp =
      if n < 0 then
        false
      else
        case todoPairs of
          (l /\ r) : otherPairs ->
            if abs (l - r) <= 3 && cmp l r then
              go n otherPairs (if l > r then (>) else (<))
            else
              go (n - 1) otherPairs cmp

          Nil ->
            true

    list = List.fromFoldable $ zip (Array.drop 1 levels) levels
  in
    go dropable list (/=)