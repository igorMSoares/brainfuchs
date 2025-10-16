
module ParserProps (spec) where

import Test.Hspec
import Test.QuickCheck
import Data.Either (isRight)
import Brainfuck.Parser (parse)
import GenBalanced (genBalanced)

validChars :: String
validChars = "><+-.,[]"

onlyBf :: String -> String
onlyBf = filter (`elem` validChars)

-- ignora qualquer caractere que não pertença à linguagem
prop_parser_ignora_invalidos :: String -> Property
prop_parser_ignora_invalidos s =
  parse s === parse (onlyBf s)

-- aceita colchetes balanceados
prop_parser_aceita_balanceados :: Property
prop_parser_aceita_balanceados =
  forAll genBalanced $ \s ->
    isRight (parse s)

-- rejeita ']' sem par correspondente
prop_parser_rejeita_extra_fecha :: Positive Int -> Bool
prop_parser_rejeita_extra_fecha (Positive k) =
  case parse (replicate k ']') of
    Left _  -> True
    Right _ -> False

-- rejeita '[' sem fechamento
prop_parser_rejeita_abre_sem_fechar :: Positive Int -> Bool
prop_parser_rejeita_abre_sem_fechar (Positive k) =
  case parse (replicate k '[') of
    Left _  -> True
    Right _ -> False

spec :: Spec
spec = describe "Parser (QuickCheck)" $ do
  it "ignora caracteres inválidos (comentários)" $
    property prop_parser_ignora_invalidos
  it "aceita colchetes balanceados" $
    property prop_parser_aceita_balanceados
  it "rejeita ']' sem par" $
    property prop_parser_rejeita_extra_fecha
  it "rejeita '[' sem fechamento" $
    property prop_parser_rejeita_abre_sem_fechar
