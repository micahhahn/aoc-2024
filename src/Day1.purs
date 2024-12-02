module Day1
  ( challenge1
  ) where

import Prelude

import Challenge (Challenge)
import Data.Maybe (Maybe(..))

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
  , solution: Nothing
  }

solution1 :: String -> String
solution1 input = input