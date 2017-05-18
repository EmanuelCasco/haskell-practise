module MonsterIncSpec where

import Test.Hspec
import Control.Exception (evaluate)

import MonsterInc

---

testAll :: IO ()
testAll = hspec $ do
  describe "[Verificar punto 1]" $ do
    it "energiaDeGrito (\"AAAAAHG\", 10, True) debería ser 700" $ do
      energiaDeGrito ("AAAAAHG", 10, True) `shouldBe` 700
    it "energiaDeGrito (\"uf\", 2, False) debería ser 8" $ do
      energiaDeGrito ("uf", 2, False) `shouldBe` 8

  describe "[Verificar punto 2]" $ do
    it "Sullivan asusta a Andy de 9 años y 1.2m, el grito resultante es (\"AAAAGH\", 2, False)" $ do
      sullivan ("Andy", 9, 1.2) `shouldBe` ("AAAAGH", 2, False)
    it "Randall Boggs  asusta a Will de 10 años y 1.1m, el grito resultante es (\"¡Mamadera!\", 1, True)" $ do
      randall ("Will", 10, 1.1) `shouldBe` ("¡Mamadera!", 1, True)
    it "Chuck Norris produce siempre un grito que dice todo el abecedario, con 1000 de nivel de intensidad y siempre hace que mojen la cama" $ do
      chuck ("Andy", 9, 1.2) `shouldBe` (['a'..'z'], 1000, True)
    it "Osito Cariñoso asusta a Andy de 9 años y 1.2m, el grito resultante es (\"uf\", 9, False)" $ do
      osito ("Andy", 9, 1.2) `shouldBe` ("uf", 9, False)

  describe "[Verificar punto 3]" $ do
    it "Hacer una función que reciba una lista de funciones y un elemento, y devuelva la lista que resulta de aplicar cada función sobre el elemento" $ do
      pam [(*2), (*3), id] 9 `shouldBe` [18, 27, 9]

  describe "[Verificar punto 4]" $ do
    it " Los monstruos a veces trabajan en equipo, por lo que van varios a la casa de un niño y todos lo asustan" $ do
      gritos ("kevin", 2, 1.1) [sullivan, osito, chuck] `shouldBe` [("AAAAAGH", 10, True), ("uf",2,False), (['a'..'z'],1000,True)]

  describe "[Verificar punto 5]" $ do
    it "Producción energética de un equipo de monstruos que asusta a un grupo de niños" $ do
      produccionEnergeticaGritos [sullivan, osito, chuck] [("Andy", 9, 1.2), ("Will", 10, 1.1)] `shouldBe` 52000071

  describe "[Verificar punto 6]" $ do
    it "Producción energética de un equipo de comediantes que divierte a un grupo de niños" $ do
      produccionEnergeticaRisas [stiller, politico] [("Andy", 9, 1.2), ("Will", 10, 1.1)] `shouldBe` 10387420491