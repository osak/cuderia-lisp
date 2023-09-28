module Cuderia.Syntax.Parser
  ( Cuderia.Syntax.Parser.parse,
    Cuderia.Syntax.Parser.ParseError,
    Cuderia.Syntax.Parser.errorPos,
    Identifier(..),
    Construct(..),
    SExpr(..),
    Program(..)
  )
where

import Data.Text qualified as T
import Text.Parsec as Parsec hiding (string)

type ParseError = Parsec.ParseError
errorPos :: Parsec.ParseError -> (Int, Int)
errorPos err = let pos = Parsec.errorPos err
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

newtype SExpr = Apply [Construct]
  deriving (Show)

data Program = Program { exprs :: [SExpr] }
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

sexpr :: CuderiaParsec SExpr
sexpr = do
  spaces
  between (char '(' >> spaces) (char ')') $ do
    constructs <- many (construct <* spaces)
    pure $ Apply constructs

program :: CuderiaParsec Program
program = do
  spaces
  exprs <- many sexpr
  spaces
  eof
  pure $ Program exprs

parse :: String -> T.Text -> Either Cuderia.Syntax.Parser.ParseError Program
parse name t = Parsec.runParser program (ParserState ()) name t