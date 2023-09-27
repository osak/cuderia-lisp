import Cuderia.Syntax.LexerSpec as LexerSpec
import Cuderia.Syntax.ParserSpec as ParserSpec
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
  defaultMain $
    testGroup
      "tests"
      [ ParserSpec.tests,
        LexerSpec.tests
      ]
