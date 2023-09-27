module Cuderia.Syntax.Parser
  ( Cuderia.Syntax.Parser.parse,
    Cuderia.Syntax.Parser.ParseError,
    Cuderia.Syntax.Parser.errorPos,
    Identifier(..),
    Construct(..),
    SExpr(..)
  )
where

import Data.Text qualified as T
import Text.Parsec as Parsec

type ParseError = Parsec.ParseError
errorPos :: Parsec.ParseError -> (Int, Int)
errorPos err = let pos = Parsec.errorPos err
    in (sourceLine pos, sourceColumn pos)

newtype Identifier = Identifier T.Text
  deriving (Show)

data Construct
  = Var Identifier
  | Integer Int
  | Expr SExpr
  deriving (Show)

newtype SExpr = SExpr [Construct]
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
    _ -> pure $ Integer absValue

construct :: Parsec T.Text () Construct
construct = expr <|> var <|> integer
  where
    expr = spaces >> fmap Expr sexpr
    var = spaces >> fmap Var identifier

sexpr :: Parsec T.Text () SExpr
sexpr = do
  spaces
  between (char '(' >> spaces) (char ')') $ do
    constructs <- many (construct <* spaces)
    pure $ SExpr constructs

program :: Parsec T.Text () [SExpr]
program = do
  spaces
  exprs <- many sexpr
  spaces
  eof
  pure exprs

parse :: String -> T.Text -> Either Cuderia.Syntax.Parser.ParseError [SExpr]
parse name t = Parsec.runParser program () name t