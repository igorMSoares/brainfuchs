module GenBalanced (genBalanced) where
import Test.QuickCheck

-- Gera strings balanceadas: "", "[]", "[[]]", "[][]", etc.
genBalanced :: Gen String
genBalanced = sized go
  where
    go 0 = pure ""
    go n = oneof
      [ pure ""
      , do a <- go (n `div` 2)
           b <- go (n `div` 2)
           pure $ "[" ++ a ++ "]" ++ b
      ]
