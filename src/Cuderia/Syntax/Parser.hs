module Cuderia.Syntax.Parser
  ( Cuderia.Syntax.Parser.parse,
    Cuderia.Syntax.Parser.ParseError,
    Identifier (..),
    Construct (..),
    SExpr (..),
  )
where

import Data.Text qualified as T
import Text.Parsec as Parsec
import Text.Parsec.Error

type ParseError = Text.Parsec.Error.ParseError

newtype Identifier = Identifier T.Text
  deriving (Show)

data Construct
  = Var Identifier
  | Integer Int
  | SExpr SExpr
  deriving (Show)

data SExpr
  = App Identifier [Construct]
  deriving (Show)

identifier :: Parsec T.Text () Identifier
identifier = do
  first <- letter
  rest <- many (letter <|> digit)
  pure $ Identifier (T.singleton first <> T.pack rest)

integer :: Parsec T.Text () Construct
integer = do
  minus <- optionMaybe (char '-')
  digits <- many1 digit
  notFollowedBy letter
  let absValue = read digits :: Int
  case minus of
    Just '-' -> pure $ Integer (-absValue)
    Nothing -> pure $ Integer absValue

construct :: Parsec T.Text () Construct
construct = try csexpr <|> var <|> integer
  where
    csexpr = spaces >> fmap SExpr sexpr
    var = spaces >> fmap Var identifier

sexpr :: Parsec T.Text () SExpr
sexpr = do
  spaces
  between (char '(' >> spaces) (char ')') $ do
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