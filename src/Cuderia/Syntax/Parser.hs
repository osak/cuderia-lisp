{-# LANGUAGE OverloadedStrings #-}

module Cuderia.Syntax.Parser
  ( Cuderia.Syntax.Parser.parse,
    Cuderia.Syntax.Parser.ParseError,
    Cuderia.Syntax.Parser.errorPos,
    Identifier (..),
    Construct (..),
    SExpr (..),
    Program (..),
  )
where

import Data.Text qualified as T
import Text.Parsec as Parsec hiding (string)

type ParseError = Parsec.ParseError

errorPos :: Parsec.ParseError -> (Int, Int)
errorPos err =
  let pos = Parsec.errorPos err
   in (sourceLine pos, sourceColumn pos)

newtype Identifier = Identifier T.Text
  deriving (Show)

data Construct
  = Var Identifier
  | Integer Int
  | String T.Text
  | Slot Int
  | Expr SExpr
  deriving (Show)

data SExpr
  = Apply [Construct]
  | Let [(Identifier, Construct)] SExpr
  | Lambda [Identifier] SExpr
  deriving (Show)

data Program = Program {exprs :: [SExpr]}
  deriving (Show)

data ParserState = ParserState ()

type CuderiaParsec a = Parsec T.Text ParserState a

idLetter :: CuderiaParsec Char
idLetter = letter <|> oneOf "+-*!"

identifier :: CuderiaParsec Identifier
identifier = do
  first <- idLetter
  rest <- many (idLetter <|> digit)
  pure $ Identifier (T.singleton first <> T.pack rest)

integer :: CuderiaParsec Construct
integer = do
  minus <- optionMaybe (char '-')
  digits <- many1 digit
  notFollowedBy letter
  let absValue = read digits :: Int
  case minus of
    Just '-' -> pure $ Integer (-absValue)
    _ -> pure $ Integer absValue

string :: CuderiaParsec Construct
string = do
  _ <- char '"'
  letters <- many (noneOf "\"")
  _ <- char '"'
  pure . String $ T.pack letters

construct :: CuderiaParsec Construct
construct = expr <|> var <|> integer <|> string <|> slot
  where
    expr = fmap Expr sexpr
    var = fmap Var identifier
    slot = do
      _ <- char '\''
      (Integer i) <- integer
      pure $ Slot i

slist :: CuderiaParsec a -> CuderiaParsec [a]
slist itemParsec = do
  spaces
  between (char '(' >> spaces) (char ')') $ do
    many (itemParsec <* spaces)

doLet :: CuderiaParsec SExpr
doLet = do
  spaces
  bindings <- slist binding
  spaces
  body <- sexpr
  pure $ Let bindings body
  where
    binding :: CuderiaParsec (Identifier, Construct)
    binding = do
      between (char '(' >> spaces) (char ')') $ do
        name <- identifier
        spaces
        val <- construct
        spaces
        pure (name, val)

doLambda :: CuderiaParsec SExpr
doLambda = do
  spaces
  args <- slist identifier
  spaces
  body <- sexpr
  pure $ Lambda args body

sexpr :: CuderiaParsec SExpr
sexpr = do
  spaces
  between (char '(' >> spaces) (char ')') $ do
    first <- optionMaybe identifier
    case first of
      Nothing -> fail "Invalid S-expression"
      Just (Identifier "let") -> doLet
      Just (Identifier "lambda") -> doLambda
      Just name -> doApply name
  where
    doApply :: Identifier -> CuderiaParsec SExpr
    doApply first = do
      spaces
      args <- many (construct <* spaces)
      pure $ Apply (Var first : args)

program :: CuderiaParsec Program
program = do
  spaces
  exprs <- many sexpr
  spaces
  eof
  pure $ Program exprs

parse :: String -> T.Text -> Either Cuderia.Syntax.Parser.ParseError Program
parse name t = Parsec.runParser program (ParserState ()) name t