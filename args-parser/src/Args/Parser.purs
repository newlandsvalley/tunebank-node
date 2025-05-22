module Args.Parser 
  ( parse
  ) where

import Prelude

import Data.Either (Either)
import StringParser (Parser, ParseError, runParser, try)
import StringParser.CodePoints (string, whiteSpace)
import StringParser.Combinators (choice)
import Args.Types (IncomingGenre(..), Target(..))


target :: Parser Target
target =
   whiteSpace *> choice [users , try tunes , comments]

users :: Parser Target 
users = 
  Users <$ string "users"

tunes :: Parser Target 
tunes = 
  Tunes <$> genre <* (whiteSpace <* string "tunes")

comments :: Parser Target 
comments = 
  Comments <$> genre <* (whiteSpace <* string "comments")

genre :: Parser IncomingGenre 
genre = 
  choice 
    [ English <$ string "english"
    , Irish <$ string "irish"
    , Klezmer <$ string "klezmer"
    , Scandi <$ string "scandi"
    , Scottish <$ string "scottish"
    ]

parse :: String -> Either ParseError Target
parse s =
  runParser target s 
