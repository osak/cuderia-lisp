{-# LANGUAGE OverloadedStrings #-}
module Cuderia.VM.InterpreterSpec (
    tests
) where

import Cuderia.Syntax.Parser
import Cuderia.VM.Interpreter
import Data.Text qualified as T
import Test.Tasty
import Test.Tasty.HUnit
import Data.Either (fromRight)

tests :: TestTree
tests = testCase "Addition" $ do
    let sexpr = fromRight undefined $ parse "Addition" "(+ 1 2 3)"
    let interpreter = newInterpreter
    let result = runInterpreter interpreter (head sexpr)
    case result of
        Left err -> assertFailure $ "Interpreter failed: " ++ show err
        Right v -> case v of
            IntValue i -> 6 @=? i
            other -> assertFailure $ "Interpreter returned wrong value: " ++ display other