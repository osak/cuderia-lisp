{-# LANGUAGE OverloadedStrings #-}

module Cuderia.Syntax.ParserSpec
  ( tests,
  )
where

import Cuderia.Syntax.Parser
import Cuderia.Syntax.Token
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

tests :: TestTree
tests =
  testGroup
    "Parser tests"
    [ parserTestCase "Simple" "(cons 1 2)" "(cons 1 2)",
      parserTestCase "Nested" "(cons (cons 1 2) 3)" "(cons (cons 1 2) 3)",
      parserTestCase "Ignore spaces" "(  cons  foo  bar )" "(cons foo bar)"
    ]