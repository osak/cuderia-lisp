{-# LANGUAGE OverloadedStrings #-}

module Cuderia.VM.Interpreter (
    Interpreter,
    Value(..),
    CuderiaError(..),
    display,
    newInterpreter,
    runInterpreter
) where

import Cuderia.Syntax.Parser as P
import Data.Array
import Data.Text qualified as T

data Value e
  = Nil
  | IntValue Int
  | StringValue T.Text
  | CellRef e Int

display :: Value e -> String
display Nil = "(nil)"
display (IntValue i) = show i
display (StringValue s) = show s
display (CellRef _ i) = "(ref to " ++ show i ++ ")"

intValue :: Value e -> Int
intValue (IntValue i) = i
intValue v = error $ "Bug: IntValue expected but got " ++ display v

data Cell e = Cell {car :: Value e, cdr :: Value e}

data CuderiaError
  = InvalidFunctionError String
  | InvalidInvocationError String
  deriving (Show)

data Interpreter e = Interpreter {cells :: Array Int (Cell e)}

newInterpreter :: Interpreter e
newInterpreter = Interpreter {cells = array (0, 1000) []}

runInterpreter :: Interpreter e -> SExpr -> Either CuderiaError (Value e)
runInterpreter i sexpr = evaluateSExpr sexpr

apply :: Identifier -> [Construct] -> Either CuderiaError (Value e)
apply ident args = case ident of
  Identifier "+" ->
    foldl
      ( \acc v -> do
          cur <- acc
          val <- evaluateConstruct v
          case val of
            IntValue i -> pure $ IntValue (intValue cur + i)
            _ -> Left $ InvalidInvocationError $ display val ++ " is not a number"
      )
      (Right (IntValue 0))
      args
  _ -> undefined

evaluateConstruct :: Construct -> Either CuderiaError (Value e)
evaluateConstruct (Integer i) = Right $ IntValue i
evaluateConstruct (String s) = Right $ StringValue s
evaluateConstruct _ = Left $ InvalidInvocationError "Not supported"

evaluateSExpr :: SExpr -> Either CuderiaError (Value e)
evaluateSExpr (SExpr []) = Right Nil
evaluateSExpr (SExpr (f : args)) = case f of
  Var ident -> apply ident args
  _ -> Left $ InvalidFunctionError (show f ++ " is not a function")