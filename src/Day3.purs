module Day3 where

import Prelude

import Challenge (Challenge)
import Data.Foldable (sum)
import Data.Maybe (Maybe(..))
import Data.Tuple as Tuple
import Parser (Parser, string, crashLeft, fixedNumber, anyTill, manyArray, runParser)

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

solution1 :: String -> String
solution1 input =
  let
    result = runParser input parser # crashLeft
  in
    result
      # map (\(Mul l r) -> l * r)
      # sum
      # show

data Mul = Mul Int Int

instance Show Mul where
  show (Mul l r) = show l <> "*" <> show r

mulParser :: Parser String Mul
mulParser = do
  _ <- string "mul("
  l <- fixedNumber 1 3
  _ <- string ","
  r <- fixedNumber 1 3
  _ <- string ")"
  pure (Mul l r)

parser ∷ Parser String (Array Mul)
parser =
  manyArray (anyTill mulParser <#> Tuple.snd)