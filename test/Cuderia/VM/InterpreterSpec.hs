{-# LANGUAGE OverloadedStrings #-}

module Cuderia.VM.InterpreterSpec
  ( tests,
  )
where

import Cuderia.Syntax.Parser
import Cuderia.VM.Interpreter
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

shouldReturn :: String -> T.Text -> Value -> TestTree
shouldReturn name src expected =
  shouldSuccess
    name
    src
    (expected @=?)

shouldFail :: String -> T.Text -> (CuderiaError -> Assertion) -> TestTree
shouldFail name src eval = testCase name $ do
  let program = fromRight undefined $ parse name src
  let (_, result) = runInterpreter newInterpreter program
  case result of
    Right val -> assertFailure . T.unpack $ "Interpreter succeeded with " <> display val <> " - should have failed"
    Left err -> eval err

tests :: TestTree
tests =
  testGroup
    "Interpreter"
    [ shouldReturn "Addition" "(+ 1 2 3)" (IntValue 6),
      shouldReturn "Subtraction" "(- 10 3 2)" (IntValue 5),
      shouldReturn "Multiplication" "(* 1 2 3 4 5)" (IntValue 120),
      shouldReturn "Set and Get" "(do (set! '0 1) (get! '0))" (IntValue 1),
      shouldReturn "Let" "(let ((x 1) (y 2)) (+ x y))" (IntValue 3),
      shouldReturn "Lambda" "(let ((f (lambda (x) (+ x 1)))) (f 10))" (IntValue 11),
      shouldReturn "If-true" "(if (< 0 1) 1 2)" (IntValue 1),
      shouldReturn "If-false" "(if (< 1 0) 1 2)" (IntValue 2),
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