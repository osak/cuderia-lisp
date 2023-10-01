{-# LANGUAGE OverloadedStrings #-}

module Cuderia.VM.Interpreter
  ( Interpreter,
    Value (..),
    CuderiaError (..),
    display,
    interpret,
    newInterpreter,
    runInterpreter,
  )
where

import Control.Monad
import Cuderia.Syntax.Parser as P
import Data.Array
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Text qualified as T

type Symbol = T.Text

data Value
  = Nil
  | IntValue Int
  | StringValue T.Text
  | BoolValue Bool
  | CellRef Int
  | Function [Symbol] SExpr

display :: Value -> String
display Nil = "(nil)"
display (IntValue i) = show i
display (StringValue s) = show s
display (BoolValue b) = show b
display (CellRef i) = "(ref to " ++ show i ++ ")"

intValue :: Value -> Int
intValue (IntValue i) = i
intValue v = error $ "Bug: IntValue expected but got " ++ display v

data Cell = Cell {car :: Value, cdr :: Value}

data CuderiaError
  = InvalidFunctionError String
  | InvalidInvocationError String
  | UndefinedVariableError T.Text
  deriving (Show)

data Environment = Environment {slots :: Array Int Value, vars :: Map.Map T.Text Value, currentError :: Maybe CuderiaError}

newEnvironment :: Environment
newEnvironment = Environment (array (0, 1000) []) Map.empty Nothing

newtype Evaluation a = Evaluation {runEvaluation :: Environment -> (Environment, Maybe a)}

getSlot :: Int -> Evaluation Value
getSlot slot = Evaluation $ \rep -> (rep, Just $ slots rep ! slot)

setSlot :: Int -> Value -> Evaluation Value
setSlot slot val = Evaluation $ \rep -> (rep {slots = slots rep // [(slot, val)]}, Just val)

setVar :: T.Text -> Value -> Evaluation Value
setVar name val = Evaluation $ \rep -> (rep {vars = Map.insert name val $ vars rep}, Just val)

getVar :: T.Text -> Evaluation Value
getVar name = Evaluation $ \rep -> case Map.lookup name (vars rep) of
  Just val -> runEvaluation (pure val) rep
  Nothing -> runEvaluation (raise $ UndefinedVariableError $ "Undefined variable " <> name) rep

raise :: CuderiaError -> Evaluation a
raise err = Evaluation $ \rep -> (rep {currentError = Just err}, Nothing)

getError :: Evaluation CuderiaError
getError = Evaluation $ \rep -> (rep, currentError rep)

runFork :: Evaluation Value -> Evaluation Value
runFork e = Evaluation $ \rep -> case runEvaluation e rep of
  (_, Just x) -> (rep, Just x)
  (new_rep, Nothing) -> (new_rep, Nothing) -- In case of evaluation error, return the updated rep to provide useful context

instance Monad Evaluation where
  return :: a -> Evaluation a
  return a = Evaluation $ \r -> (r, Just a)
  a >>= f = Evaluation $ \r -> case currentError r of
    Just err -> (r, Nothing)
    Nothing ->
      let (r_1, maybeV) = runEvaluation a r
       in case maybeV of
            Nothing -> (r_1, Nothing)
            Just v -> runEvaluation (f v) r_1

instance Functor Evaluation where
  fmap fab ma = do a <- ma; return (fab a)

instance Applicative Evaluation where
  pure a = do return a
  mfab <*> ma = do fab <- mfab; fab <$> ma

newtype Interpreter = Interpreter {environment :: Environment}

newInterpreter :: Interpreter
newInterpreter = Interpreter newEnvironment

runInterpreter :: Interpreter -> Program -> (Interpreter, Either CuderiaError Value)
runInterpreter ip program = case run of
  Left (rep, err) -> (Interpreter rep, Left err)
  Right (rep, val) -> (Interpreter rep, Right val)
  where
    run :: Either (Environment, CuderiaError) (Environment, Value)
    run =
      foldM
        ( \(rep, _) sexpr -> do
            let (newrep, ret) = runEvaluation (evaluateSExpr sexpr) rep
            case ret of
              Just x -> Right (newrep, x)
              Nothing -> Left (newrep, fromJust $ currentError newrep)
        )
        (environment ip, Nil)
        (exprs program)

interpret :: SExpr -> Either CuderiaError Value
interpret sexpr =
  let (envrep, ret) = runEvaluation (evaluateSExpr sexpr) newEnvironment
   in case currentError envrep of
        Just err -> Left err
        Nothing -> Right $ fromMaybe Nil ret

foldInts :: (Int -> Int -> Int) -> [Construct] -> Evaluation Value
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

apply :: Identifier -> [Construct] -> Evaluation Value
apply ident args = case ident of
  Identifier "+" -> foldInts (+) args
  Identifier "-" -> foldInts (-) args
  Identifier "*" -> foldInts (*) args
  Identifier "<" -> do
    let (op1:op2:_) = args
    v1 <- evaluateConstruct op1
    v2 <- evaluateConstruct op2
    pure $ BoolValue ((intValue v1) < (intValue v2))
  Identifier "set!" ->
    let ((Slot slot) : construct : _) = args
     in do
          val <- evaluateConstruct construct
          setSlot slot val
  Identifier "get!" ->
    let ((Slot slot) : _) = args
     in getSlot slot
  Identifier "do" -> do
    results <- mapM evaluateConstruct args
    pure $ last results
  Identifier name -> do
    maybeFunc <- getVar name
    case maybeFunc of
        Function params body -> do
            let arity = length params
            if arity /= length args then
                raise $ InvalidInvocationError $ T.unpack $ name <> " expects " <> (T.pack $ show arity) <> " arguments but called with " <> (T.pack $ show $ length args) <> " arguments"
            else
                runFork $ do
                    forM_ (zip params args) (\(name, val) -> evaluateConstruct val >>= setVar name)
                    evaluateSExpr body
        _ -> raise $ InvalidFunctionError $ T.unpack $ name <> " is not a function"
  _ -> undefined

evaluateConstruct :: Construct -> Evaluation Value
evaluateConstruct (Integer i) = pure $ IntValue i
evaluateConstruct (String s) = pure $ StringValue s
evaluateConstruct (Expr expr) = evaluateSExpr expr
evaluateConstruct (Var (Identifier name)) = getVar name
evaluateConstruct _ = raise $ InvalidInvocationError "Not supported"

evaluateSExpr :: SExpr -> Evaluation Value
evaluateSExpr (Apply []) = pure Nil
evaluateSExpr (Apply (f : args)) = case f of
  Var ident -> apply ident args
  _ -> raise $ InvalidFunctionError (show f ++ " is not a function")
evaluateSExpr (Let bindings body) = do
  runFork $ do
    forM_ bindings (\(Identifier name, val) -> evaluateConstruct val >>= setVar name)
    evaluateSExpr body
evaluateSExpr (Lambda args body) = do
    let argsyms = map (\(Identifier i) -> i) args
    pure $ Function argsyms body
evaluateSExpr (If cond body1 body2) = do
    condVal <- evaluateConstruct cond
    case condVal of
        BoolValue True -> evaluateConstruct body1
        BoolValue False -> evaluateConstruct body2
        _ -> raise $ InvalidInvocationError $ display condVal ++ " is not a boolean, thus incompatible for the condition of `if` statement."