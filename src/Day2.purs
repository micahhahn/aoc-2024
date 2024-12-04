module Day2
  ( Report(..)
  , challenge1
  , challenge2
  , isSafeN
  ) where

import Prelude

import Challenge (Challenge)
import Data.Array (zip)
import Data.Array as Array
import Data.Foldable (sum)
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Ord (abs)
import Data.Lazy (defer, force)
import Data.String as String
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

challenge2 :: Challenge
challenge2 =
  { name: "Day 2 Part 2"
  , examplePrompt: challenge1.examplePrompt
  , exampleAnswer: "4"
  , solver: solution2
  , promptPath: "assets/day2.txt"
  , solution: Just "337"
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

isSafeN :: Int -> Report -> Boolean
isSafeN dropable (Report levels) =
  let
    go n todoPairs cmp =
      if n < 0 then
        false
      else
        case todoPairs of
          (l1 /\ r1) : (l2 /\ r2) : otherPairs ->
            if abs (l1 - r1) <= 3 && cmp l1 r1 then
              force $
                defer (\_ -> go n ((l2 /\ r2) : otherPairs) (if l1 > r1 then (>) else (<)))
                  || defer (\_ -> go (n - 1) ((l1 /\ r2) : otherPairs) cmp)
            else
              go (n - 1) ((l1 /\ r2) : otherPairs) cmp

          (l /\ r) : Nil ->
            (abs (l - r) <= 3 && cmp l r)
              || go (n - 1) Nil cmp

          Nil ->
            true

    list = List.fromFoldable $ zip levels (Array.drop 1 levels)
  in
    force $
      defer (\_ -> go dropable list (/=))
        || defer (\_ -> go (dropable - 1) (List.drop 1 list) (/=))

solveN :: Int -> String -> String
solveN droppable input =
  parse input
    # map (\x -> if isSafeN droppable x then 1 else 0)
    # sum
    # show

solution1 :: String -> String
solution1 = solveN 0

solution2 :: String -> String
solution2 = solveN 1