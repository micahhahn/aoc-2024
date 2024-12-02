module Parser
  ( crashLeft
  , module Parsing
  , module Parsing.Combinators
  , number
  , space
  , sepByArray
  , newline
  ) where

import Prelude

import Data.Array (fromFoldable)
import Data.Char (toCharCode)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.List.Types (toList)
import Parsing (ParseError(..), Parser, Position(..), position, runParser, ParserT)
import Parsing.Combinators (many1, sepBy)
import Parsing.String (char, satisfy)
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

space :: Parser String Char
space = char ' '

sepByArray :: ∀ m s a sep. ParserT s m a -> ParserT s m sep -> ParserT s m (Array a)
sepByArray p1 p2 = sepBy p1 p2 <#> fromFoldable

newline :: Parser String Char
newline = char '\n'

crashLeft :: ∀ a. Either ParseError a -> a
crashLeft result =
  case result of
    Left err ->
      unsafeCrashWith (show err)
    Right v ->
      v
