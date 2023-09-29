{-# LANGUAGE OverloadedStrings #-}

module Cuderia.VM.InterpreterSpec
  ( tests,
  )
where

import Cuderia.Syntax.Parser
import Cuderia.VM.Interpreter
import Cuderia.VM.Interpreter (CuderiaError (UndefinedVariableError))
import Data.Either (fromRight)
import Data.Text qualified as T
import Test.Tasty
import Test.Tasty.HUnit

shouldSuccess :: String -> T.Text -> (Value -> Assertion) -> TestTree
shouldSuccess name src eval = testCase name $ do
  let program = fromRight undefined $ parse name src
  let (_, result) = runInterpreter newInterpreter program
  case result of
    Right val -> eval val
    Left err -> assertFailure $ "Interpreter failed: " ++ show err

shouldDoMath :: String -> T.Text -> Int -> TestTree
shouldDoMath name src expected =
  shouldSuccess
    name
    src
    ( \v -> case v of
        IntValue i -> expected @=? i
        other -> assertFailure $ "Interpreter returned wrong value: " ++ display other
    )

shouldFail :: String -> T.Text -> (CuderiaError -> Assertion) -> TestTree
shouldFail name src eval = testCase name $ do
  let program = fromRight undefined $ parse name src
  let (_, result) = runInterpreter newInterpreter program
  case result of
    Right val -> assertFailure $ "Interpreter succeeded with " ++ display val ++ " - should have failed"
    Left err -> eval err

tests :: TestTree
tests =
  testGroup
    "Interpreter"
    [ shouldDoMath "Addition" "(+ 1 2 3)" 6,
      shouldDoMath "Subtraction" "(- 10 3 2)" 5,
      shouldDoMath "Multiplication" "(* 1 2 3 4 5)" 120,
      shouldDoMath "Set and Get" "(do (set! '0 1) (get! '0))" 1,
      shouldDoMath "Let" "(let ((x 1) (y 2)) (+ x y))" 3,
      shouldDoMath "Lambda" "(let ((f (lambda (x) (+ x 1)))) (f 10))" 11,
      shouldDoMath "If-true" "(if (< 0 1) (+ 1 0) (+ 2 0))" 1,
      shouldDoMath "If-false" "(if (< 1 0) (+ 1 0) (+ 2 0))" 2,
      shouldFail
        "Adding non-number"
        "(+ 1 \"foo\")"
        ( \err -> case err of
            InvalidInvocationError msg -> msg @=? "\"foo\" is not a number"
            _ -> assertFailure $ "Interpreter failed with an unexpected error: " ++ show err
        ),
      shouldFail
        "Lookup undefined var"
        "(+ 1 a)"
        ( \err -> case err of
            UndefinedVariableError msg -> msg @=? "Undefined variable a"
            _ -> assertFailure $ "Interpreter failed with an unexpected error: " ++ show err
        ),
      shouldFail
        "Lookup outside of let should fail"
        "(do (let ((x 1)) (+ 1)) (+ x))"
        ( \err -> case err of
            UndefinedVariableError msg -> msg @=? "Undefined variable x"
            _ -> assertFailure $ "Interpreter failed with an unexpected error: " ++ show err
        )
    ]