{-# LANGUAGE OverloadedStrings #-}

module Cuderia.Syntax.LexerSpec
  ( tests,
  )
where

import Cuderia.Syntax.Lexer
import Data.Text qualified as T
import Test.Tasty
import Test.Tasty.HUnit
import Prelude hiding (lex)

lexerTestCase :: String -> T.Text -> [Token] -> TestTree
lexerTestCase name src expected = testCase name $ evaluate (lex name src)
  where
    evaluate (Left err) = assertFailure $ "Lex error" <> show err
    evaluate (Right tokens) = tokens @=? expected

lexerFailureCase :: String -> T.Text -> (Int, Int) -> TestTree
lexerFailureCase name src pos = testCase name $ evaluate (lex name src)
  where
    evaluate :: Either LexError [Token] -> Assertion
    evaluate (Left err) = pos @=? errorPos err
    evaluate (Right tokens) = assertFailure $ "Lexer succeeded with " ++ (show tokens) ++ " - should have failed"

tests :: TestTree
tests =
  testGroup
    "Lexer tests"
    [ lexerTestCase "Simple" "(cons 1 foo)" [ParenOpen, Identifier "cons", Integer 1, Identifier "foo", ParenClose],
      lexerTestCase "Empty" "()" [ParenOpen, ParenClose],
      lexerTestCase "Identifier with digits" "foo123" [Identifier "foo123"],
      lexerFailureCase "Digits followed by alphabet" "123foo" (1, 5)
    ]