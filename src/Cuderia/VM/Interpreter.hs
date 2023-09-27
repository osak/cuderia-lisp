{-# LANGUAGE OverloadedStrings #-}

module Cuderia.VM.Interpreter
  ( Interpreter,
    Value (..),
    CuderiaError (..),
    display,
    interpret,
  )
where

import Cuderia.Syntax.Parser as P
import Data.Array
import Data.Maybe
import Data.Text qualified as T

data Value
  = Nil
  | IntValue Int
  | StringValue T.Text
  | CellRef Int

display :: Value -> String
display Nil = "(nil)"
display (IntValue i) = show i
display (StringValue s) = show s
display (CellRef i) = "(ref to " ++ show i ++ ")"

intValue :: Value -> Int
intValue (IntValue i) = i
intValue v = error $ "Bug: IntValue expected but got " ++ display v

data Cell = Cell {car :: Value, cdr :: Value}

data CuderiaError
  = InvalidFunctionError String
  | InvalidInvocationError String
  deriving (Show)

data EnvironmentRep = EnvironmentRep {slots :: Array Int Value, currentError :: Maybe CuderiaError}

newEnvironmentRep :: EnvironmentRep
newEnvironmentRep = EnvironmentRep (array (0, 1000) []) Nothing

newtype Environment a = Environment {runEnvironment :: EnvironmentRep -> (EnvironmentRep, Maybe a)}

getSlot :: Int -> Environment Value
getSlot slot = Environment $ \rep -> (rep, Just $ slots rep ! slot)

setSlot :: Int -> Value -> Environment ()
setSlot slot val = Environment $ \rep -> (EnvironmentRep (slots rep // [(slot, val)]) (currentError rep), Nothing)

raise :: CuderiaError -> Environment a
raise err = Environment $ \rep -> (EnvironmentRep (slots rep) (Just err), Nothing)

getError :: Environment CuderiaError
getError = Environment $ \rep -> (rep, currentError rep)

instance Monad Environment where
  return a = Environment $ \r -> (r, Just a)
  a >>= f = Environment $ \r -> case currentError r of
    Just err -> (r, Nothing)
    Nothing ->
      let (r_1, maybeV) = runEnvironment a r
       in case maybeV of
            Nothing -> (r_1, Nothing)
            Just v -> runEnvironment (f v) r

instance Functor Environment where
  fmap fab ma = do fab <$> ma

instance Applicative Environment where
  pure a = do return a
  mfab <*> ma = do fab <- mfab; fab <$> ma

newtype Interpreter = Interpreter {runInterpreter :: SExpr -> Either CuderiaError Value}

interpret :: SExpr -> Either CuderiaError Value
interpret sexpr =
  let (envrep, ret) = runEnvironment (evaluateSExpr sexpr) newEnvironmentRep
   in case currentError envrep of
        Just err -> Left err
        Nothing -> Right $ fromMaybe Nil ret

foldInts :: (Int -> Int -> Int) -> [Construct] -> Environment Value
foldInts _ [] = raise $ InvalidInvocationError "At least one operand must be given"
foldInts f (c : cs) = fmap IntValue result
  where
    result = do
      first <- evaluateConstruct c
      z <- case first of
        IntValue i -> pure i
        _ -> raise $ InvalidInvocationError $ display first ++ " is not a number"
      foldl
        ( \acc v -> do
            cur <- acc
            val <- evaluateConstruct v
            case val of
              IntValue i -> pure (f cur i)
              _ -> raise $ InvalidInvocationError $ display val ++ " is not a number"
        )
        (pure z)
        cs

apply :: Identifier -> [Construct] -> Environment Value
apply ident args = case ident of
  Identifier "+" -> foldInts (+) args
  Identifier "-" -> foldInts (-) args
  Identifier "*" -> foldInts (*) args
  _ -> undefined

evaluateConstruct :: Construct -> Environment Value
evaluateConstruct (Integer i) = pure $ IntValue i
evaluateConstruct (String s) = pure $ StringValue s
evaluateConstruct _ = raise $ InvalidInvocationError "Not supported"

evaluateSExpr :: SExpr -> Environment Value
evaluateSExpr (SExpr []) = pure Nil
evaluateSExpr (SExpr (f : args)) = case f of
  Var ident -> apply ident args
  _ -> raise $ InvalidFunctionError (show f ++ " is not a function")