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
    evaluate :: Either ParseError Program -> Assertion
    evaluate (Right program) = expected @=? unparseProgram program
    evaluate (Left err) = assertFailure $ "Parse error: " <> show err

parserFailureCase :: String -> T.Text -> (Int, Int) -> TestTree
parserFailureCase name src pos = testCase name $ evaluate (parse name src)
  where
    evaluate :: Either ParseError Program -> Assertion
    evaluate (Right program) = assertFailure $ "Parser succeeded with " ++ (show program) ++ " - should have failed"
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
      parserTestCase "Slot" "('12)" "('12)",
      parserFailureCase "Unclosng paren" "(cons 1 2" (1, 10),
      parserFailureCase "Unmatching paren" ")cons 1 2" (1, 1),
      parserFailureCase "Invalid number literal" "(const 123foo)" (1, 12)
    ]