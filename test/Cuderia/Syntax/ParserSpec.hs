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

parseToIdentical :: String -> T.Text -> TestTree
parseToIdentical name src = parserTestCase name src src

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
    [ parseToIdentical "Simple" "(cons 1 2)",
      parseToIdentical "Nested" "(cons (cons 1 2) 3)",
      parseToIdentical "Negative numbers" "(cons (cons -1 -2) -3)",
      parserTestCase "Ignore spaces" "(  cons  foo  bar )" "(cons foo bar)",
      parseToIdentical "No arguments" "(nil)",
      parseToIdentical "Plus" "(+ 1 2 3)",
      parseToIdentical "String" "(const \"hello\")",
      parseToIdentical "Slot" "(const '12)",
      parseToIdentical "Let" "(let ((x 1) (y 2)) (print x))",
      parseToIdentical "Lambda" "(lambda (x y) (+ x y))",
      parseToIdentical "If" "(if (< 0 1) 1 (print 0))",
      parseToIdentical "Cond" "(cond ((= x 1) 1) ((= x 2) 2))",
      parseToIdentical "Cond-else" "(cond ((= x 1) 1) ((= x 2) 2) (else 3))",
      parserFailureCase "Unclosng paren" "(cons 1 2" (1, 10),
      parserFailureCase "Unmatching paren" ")cons 1 2" (1, 1),
      parserFailureCase "Invalid number literal" "(const 123foo)" (1, 12),
      parserFailureCase "Misplaced cond-else" "(cond ((= x 1) 1) (else 2) ((= x 3) 3))" (1, 28)
    ]