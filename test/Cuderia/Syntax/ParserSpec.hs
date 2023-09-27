{-# LANGUAGE OverloadedStrings #-}

module Cuderia.Syntax.ParserSpec
  ( tests,
  )
where

import Cuderia.Syntax.Parser
import Cuderia.Syntax.Unparser
import Data.Text qualified as T
import Test.Tasty
import Test.Tasty.HUnit

parserTestCase :: String -> T.Text -> T.Text -> TestTree
parserTestCase name src expected = testCase name $ evaluate (parse name src)
  where
    evaluate :: Either ParseError [SExpr] -> Assertion
    evaluate (Right ast) = expected @=? unparse ast
    evaluate (Left err) = assertFailure $ "Parse error: " <> show err

parserFailureCase :: String -> T.Text -> (Int, Int) -> TestTree
parserFailureCase name src pos = testCase name $ evaluate (parse name src)
  where
    evaluate :: Either ParseError [SExpr] -> Assertion
    evaluate (Right ast) = assertFailure $ "Parser succeeded with " ++ (show ast) ++ " - should have failed"
    evaluate (Left err) = pos @=? errorPos err

tests :: TestTree
tests =
  testGroup
    "Parser tests"
    [ parserTestCase "Simple" "(cons 1 2)" "(cons 1 2)",
      parserTestCase "Nested" "(cons (cons 1 2) 3)" "(cons (cons 1 2) 3)",
      parserTestCase "Negative numbers" "(cons (cons -1 -2) -3)" "(cons (cons -1 -2) -3)",
      parserTestCase "Ignore spaces" "(  cons  foo  bar )" "(cons foo bar)",
      parserTestCase "No arguments" "(nil)" "(nil)",
      parserTestCase "Plus" "(+ 1 2 3)" "(+ 1 2 3)",
      parserTestCase "String" "(\"hello\")" "(\"hello\")",
      parserFailureCase "Unclosng paren" "(cons 1 2" (1, 10),
      parserFailureCase "Unmatching paren" ")cons 1 2" (1, 1),
      parserFailureCase "Invalid number literal" "(const 123foo)" (1, 12)
    ]