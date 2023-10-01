{-# LANGUAGE OverloadedStrings #-}

module Cuderia.Syntax.Parser
  ( Cuderia.Syntax.Parser.parse,
    Cuderia.Syntax.Parser.ParseError,
    Cuderia.Syntax.Parser.errorPos,
    Identifier (..),
    Term (..),
    SExpr (..),
    Program (..),
  )
where

import Control.Applicative (liftA2)
import Data.Text qualified as T
import Text.Parsec as Parsec

type ParseError = Parsec.ParseError

errorPos :: Parsec.ParseError -> (Int, Int)
errorPos err =
  let pos = Parsec.errorPos err
   in (sourceLine pos, sourceColumn pos)

newtype Identifier = Identifier T.Text
  deriving (Show, Eq)

data Term
  = Var Identifier
  | Integer Int
  | String T.Text
  | Slot Int
  | Expr SExpr
  deriving (Show, Eq)

data SExpr
  = Apply [Term]
  | Let [(Identifier, Term)] SExpr
  | Lambda [Identifier] SExpr
  | If Term Term Term
  | Cond [(Term, Term)] (Maybe Term)
  deriving (Show, Eq)

data Program = Program {exprs :: [SExpr]}
  deriving (Show)

data ParserState = ParserState ()

type CuderiaParsec a = Parsec T.Text ParserState a

idLetter :: CuderiaParsec Char
idLetter = letter <|> oneOf "+-*!<="

identifier :: CuderiaParsec Identifier
identifier = do
  first <- idLetter
  rest <- many (idLetter <|> digit)
  spaces
  let ident = T.singleton first <> T.pack rest
  case ident of
    "else" -> fail . T.unpack $ "keyword '" <> ident <> "' is not allowed at this place"
    _ -> pure $ Identifier ident

keyword :: String -> CuderiaParsec String
keyword kw = string kw <* notFollowedBy (idLetter <|> digit) <* spaces

integer :: CuderiaParsec Term
integer = do
  minus <- optionMaybe (char '-')
  digits <- many1 digit
  notFollowedBy letter
  let absValue = read digits :: Int
  case minus of
    Just '-' -> pure $ Integer (-absValue)
    _ -> pure $ Integer absValue

doString :: CuderiaParsec Term
doString = do
  _ <- char '"'
  letters <- many (noneOf "\"")
  _ <- char '"'
  pure . String $ T.pack letters

term :: CuderiaParsec Term
term = (expr <|> var <|> integer <|> doString <|> slot) <* spaces
  where
    expr = fmap Expr sexpr
    var = fmap Var identifier
    slot = do
      _ <- char '\''
      (Integer i) <- integer
      pure $ Slot i

-- Parses a list, that is, something enclosed in '(' and ')'
list :: CuderiaParsec a -> CuderiaParsec a
list inner = between (char '(' >> spaces) (char ')') inner <* spaces

list2 :: CuderiaParsec a -> CuderiaParsec b -> CuderiaParsec (a, b)
list2 pa pb = list $ liftA2 (,) pa pb

-- Parses a list of items of the same kind
slist :: CuderiaParsec a -> CuderiaParsec [a]
slist itemParsec = list (many itemParsec)

sexpr :: CuderiaParsec SExpr
sexpr = do
  list $ do
    first <- optionMaybe identifier
    case first of
      Nothing -> fail "Invalid S-expression"
      Just (Identifier "let") -> doLet
      Just (Identifier "lambda") -> doLambda
      Just (Identifier "if") -> doIf
      Just (Identifier "cond") -> doCond
      Just name -> doApply name
  where
    doApply :: Identifier -> CuderiaParsec SExpr
    doApply first = do
      args <- many term
      pure $ Apply (Var first : args)
    doLet = Let <$> slist doBinding <*> sexpr
    doBinding = list $ do
      name <- identifier
      value <- term
      pure (name, value)
    doLambda = Lambda <$> slist identifier <*> sexpr
    doIf = If <$> term <*> term <*> term
    -- (cond (cond1 body1)
    --       (cond2 body2)
    --       (else  bodye))
    doCond = do
      arms <- many (try (list2 term term))
      maybeElse <- optionMaybe (list $ keyword "else" *> term)
      pure $ Cond arms maybeElse

program :: CuderiaParsec Program
program = do
  spaces
  exprs <- many sexpr
  spaces
  eof
  pure $ Program exprs

parse :: String -> T.Text -> Either Cuderia.Syntax.Parser.ParseError Program
parse name t = Parsec.runParser program (ParserState ()) name t