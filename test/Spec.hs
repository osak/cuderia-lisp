import Cuderia.Syntax.LexerSpec as LexerSpec
import Cuderia.Syntax.ParserSpec as ParserSpec
import Cuderia.VM.InterpreterSpec as InterpreterSpec
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
  defaultMain $
    testGroup
      "tests"
      [ ParserSpec.tests,
        LexerSpec.tests,
        InterpreterSpec.tests
      ]
