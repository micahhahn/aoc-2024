module Day3 where

import Prelude

import Challenge (Challenge)
import Data.Filterable (filterMap)
import Data.Foldable (sum)
import Data.Maybe (Maybe(..))
import Data.Tuple as Tuple
import Data.Array as Array
import Data.Tuple.Nested ((/\))
import Parser (Parser, string, crashLeft, fixedNumber, anyTill, manyArray, runParser, (<|>))

challenge1 :: Challenge
challenge1 =
  { name: "Day 3 Part 1"
  , examplePrompt:
      [ "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"
      ]
  , exampleAnswer: "161"
  , solver: solution1
  , promptPath: "assets/day3.txt"
  , solution: Just "174561379"
  }

challenge2 :: Challenge
challenge2 =
  { name: "Day 3 Part 2"
  , examplePrompt:
      [ "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"
      ]
  , exampleAnswer: "48"
  , solver: solution2
  , promptPath: "assets/day3.txt"
  , solution: Nothing
  }

solution1 :: String -> String
solution1 input =
  let
    result = runParser input parser # crashLeft
  in
    result
      # filterMap
          ( \x ->
              case x of
                (Mul l r) -> Just (l * r)
                _ -> Nothing
          )
      # sum
      # show

solution2 :: String -> String
solution2 input =
  let
    result = runParser input parser # crashLeft
  in
    result
      # Array.foldl
          ( \(acc /\ enabled) instruction ->
              case instruction of
                Mul l r ->
                  if enabled then
                    (acc + (l * r)) /\ enabled
                  else
                    acc /\ enabled

                Do ->
                  acc /\ true

                Dont ->
                  acc /\ false
          )
          (0 /\ true)
      # Tuple.fst
      # show

data Instruction
  = Mul Int Int
  | Do
  | Dont

instance Show Instruction where
  show (Mul l r) = show l <> "*" <> show r
  show Do = "do"
  show Dont = "don't"

mulParser :: Parser String Instruction
mulParser = do
  _ <- string "mul("
  l <- fixedNumber 1 3
  _ <- string ","
  r <- fixedNumber 1 3
  _ <- string ")"
  pure (Mul l r)

instructionParser :: Parser String Instruction
instructionParser =
  mulParser
    <|> (string "do()" <#> const Do)
    <|> (string "don't()" <#> const Dont)

parser ∷ Parser String (Array Instruction)
parser =
  manyArray (anyTill instructionParser <#> Tuple.snd)

