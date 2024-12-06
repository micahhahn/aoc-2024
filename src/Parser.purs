module Parser
  ( crashLeft
  , fixedNumber
  , manyArray
  , module Parsing
  , module Parsing.Combinators
  , module Parsing.String
  , newline
  , notNewline
  , number
  , sepByArray
  , space
  ) where

import Prelude

import Data.Array (fromFoldable)
import Data.Char (toCharCode)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.List.Types (toList)
import Data.Tuple.Nested ((/\))
import Parsing (ParseError(..), Parser, Position(..), position, runParser, ParserT)
import Parsing.Combinators (many1, sepBy, manyIndex, many, (<|>))
import Parsing.Combinators.Array as A
import Parsing.String (char, satisfy, string, anyTill, anyChar, satisfy)
import Partial.Unsafe (unsafeCrashWith)

anyDigit :: Parser String Char
anyDigit = satisfy (\c -> c >= '0' && c <= '9')

number :: Parser String Int
number =
  many1 anyDigit <#>
    ( \digits ->
        toList digits
          # foldl (\accum c -> (toCharCode c - toCharCode '0') + accum * 10) 0
    )

fixedNumber :: Int -> Int -> Parser String Int
fixedNumber minDigits maxDigits =
  manyIndex minDigits maxDigits (const anyDigit) <#>
    ( \(_ /\ digits) ->
        foldl (\accum c -> (toCharCode c - toCharCode '0') + accum * 10) 0 digits
    )

space :: Parser String Char
space = char ' '

sepByArray :: ∀ m s a sep. ParserT s m a -> ParserT s m sep -> ParserT s m (Array a)
sepByArray p1 p2 = sepBy p1 p2 <#> fromFoldable

manyArray ∷ ∀ s m a. ParserT s m a → ParserT s m (Array a)
manyArray = A.many

newline :: Parser String Char
newline = char '\n'

notNewline :: Parser String Char
notNewline = satisfy ((/=) '\n')

crashLeft :: ∀ a. Either ParseError a -> a
crashLeft result =
  case result of
    Left err ->
      unsafeCrashWith (show err)
    Right v ->
      v
