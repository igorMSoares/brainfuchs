import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSuccess)
import qualified ParserProps
import qualified EvaluatorProps
import qualified AstProps

main :: IO ()
main = hspec $ do
  modifyMaxSuccess (const 2000) (do
      describe "Parser (QuickCheck)" ParserProps.spec
      describe "Evaluator (QuickCheck)" EvaluatorProps.spec
      describe "AST (QuickCheck)" AstProps.spec
    )