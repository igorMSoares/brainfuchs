import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSuccess)
import qualified AssemblerSpec
import qualified AstProps
import qualified EvaluatorProps
import qualified ParserProps

main :: IO ()
main = hspec $ do
  modifyMaxSuccess (const 2000) (do
      describe "AssemblerSpec" AssemblerSpec.spec
      describe "AST (QuickCheck)" AstProps.spec
      describe "Evaluator (QuickCheck)" EvaluatorProps.spec
      describe "Parser (QuickCheck)" ParserProps.spec
    )