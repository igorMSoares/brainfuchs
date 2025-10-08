import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSuccess)
import qualified EvaluatorProps

main :: IO ()
main = hspec $ do
  modifyMaxSuccess (const 200) (do
      describe "Evaluator (QuickCheck)" EvaluatorProps.spec
    )