module Cuderia.Syntax.Lexer
  ( LexError,
    Token (..),
    lex,
    errorPos,
  )
where

import Data.Functor (($>))
import Data.Text qualified as T
import Text.Parsec hiding (errorPos)
import Text.Parsec.Error qualified as PE
import Prelude hiding (lex)

type LexError = ParseError

errorPos :: LexError -> (Int, Int)
errorPos err =
  let pos = PE.errorPos err
   in (sourceLine pos, sourceColumn pos)

data Token
  = ParenOpen
  | ParenClose
  | Integer Int
  | Identifier T.Text
  deriving (Show, Eq)

parenOpen :: Parsec T.Text () Token
parenOpen = char '(' $> ParenOpen

parenClose :: Parsec T.Text () Token
parenClose = char ')' $> ParenClose

integer :: Parsec T.Text () Token
integer = try $ do
  digits <- many1 digit
  notFollowedBy letter
  pure $ Integer (read digits)

identifier :: Parsec T.Text () Token
identifier = try $ do
  first <- letter
  rest <- many alphaNum
  pure $ Identifier (T.singleton first <> T.pack rest)

lex :: String -> T.Text -> Either LexError [Token]
lex name src = runParser tokenize () name src
  where
    tokenize = many (spaces *> (parenOpen <|> parenClose <|> integer <|> identifier)) <* eof