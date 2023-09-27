{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}

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

shouldSuccess :: String -> T.Text -> (Value e -> Assertion) -> TestTree
shouldSuccess name src eval = testCase name $ do
  let sexpr = fromRight undefined $ parse name src
  let interpreter = newInterpreter
  let result = runInterpreter interpreter (head sexpr)
  case result of
    Right val -> eval val
    Left err -> assertFailure $ "Interpreter failed: " ++ show err

shouldFail :: String -> T.Text -> (CuderiaError -> Assertion) -> TestTree
shouldFail name src eval = testCase name $ do
  let sexpr = fromRight undefined $ parse name src
  let interpreter = newInterpreter
  let result = runInterpreter interpreter (head sexpr)
  case result of
    Right val -> assertFailure $ "Interpreter succeeded with " ++ display val ++ " - should have failed"
    Left err -> eval err

tests :: TestTree
tests =
  testGroup
    "Interpreter"
    [ shouldSuccess
        "Addition"
        "(+ 1 2 3)"
        ( \v -> case v of
            IntValue i -> 6 @=? i
            other -> assertFailure $ "Interpreter returned wrong value: " ++ display other
        ),
      shouldFail
        "Adding non-number"
        "(+ 1 \"foo\")"
        ( \err -> case err of
            InvalidInvocationError msg -> msg @=? "\"foo\" is not a number"
            _ -> assertFailure $ "Interpreter failed with an unexpected error: " ++ show err
        )
    ]