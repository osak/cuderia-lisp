module Cuderia.Syntax.Parser
  ( Cuderia.Syntax.Parser.parse,
    Cuderia.Syntax.Parser.ParseError,
  )
where

import Cuderia.Syntax.Token
import Data.Text qualified as T
import Text.Parsec as Parsec
import Text.Parsec.Error qualified

type ParseError = Text.Parsec.Error.ParseError

idChar :: Parsec T.Text () Char
idChar = letter <|> digit

identifier :: Parsec T.Text () Identifier
identifier = do
  chars <- many1 idChar
  pure $ Identifier (T.pack chars)

construct :: Parsec T.Text () Construct
construct = try csexpr <|> var
  where
    csexpr = spaces >> fmap SExpr sexpr
    var = spaces >> fmap Var identifier

sexpr :: Parsec T.Text () SExpr
sexpr = do
  spaces
  between (char '(' >> spaces) (spaces >> char ')') $ do
    f <- identifier
    spaces
    args <- many (construct <* spaces)
    pure $ App f args

program :: Parsec T.Text () [SExpr]
program = do
  spaces
  exprs <- many sexpr
  spaces
  eof
  pure exprs

parse :: String -> T.Text -> Either Cuderia.Syntax.Parser.ParseError [SExpr]
parse name t = Parsec.runParser program () name t