module NightClubSpec where

import Test.Hspec
import NightClub

---

testAll :: IO ()
testAll = hspec $ do
  describe "[Verificar puntos 1 y 2] " $ do
    it "Nombre de Rodri debe ser 'Rodri'" $ do
      (nombre rodri) `shouldBe` "Rodri"
    it "Resistencia de Ana debe ser 120" $ do
      (resistencia ana) `shouldBe` 120
    it "Resistencia de Ana debe ser 120" $ do
      (amigos marcos) `shouldMatchList` [rodri]

  describe "[Verificar punto 3] " $ do
    it "Cristian debe estar 'duro'" $ do
      (comoEsta cristian) `shouldBe` "duro"
    it "Rodri debe estar 'fresco'" $ do
      (comoEsta rodri) `shouldBe` "fresco"
    it "Marcos debe estar 'duro'" $ do
      (comoEsta marcos) `shouldBe` "duro"

  describe "[Verificar punto 4] " $ do
    it "Cristian reconoce a Marcos como amigo" $ do
      amigos (reconocerAmigo marcos cristian) `shouldMatchList` [marcos]
    it "Cristian no puede reconocerse a si mismo como amigo" $ do
      (reconocerAmigo cristian cristian) `shouldBe` cristian
    it "Cristian no puede reconocerse a Marcos dos veces como amigo" $ do
      reconocerAmigo marcos (reconocerAmigo marcos cristian) `shouldBe` reconocerAmigo marcos cristian

  describe "[Verificar punto 5] " $ do
    it "Ana toma grogXD. Queda con resistencia 0" $ do
      (resistencia.grogXD) ana `shouldBe` 0

    it "Si Ana toma la jarraLoca. Marcos queda con resistencia 30 (-10)" $ do
      (resistencia.head.amigos.jarraLoca) ana `shouldBe` 30
    it "Marcos toma la jarraLoca. Queda con resistencia 30" $ do
      (resistencia.jarraLoca) marcos `shouldBe` 30
    it "Rodri toma la jarraLoca. Queda con resistencia 110" $ do
      (resistencia.jarraLoca) rodri `shouldBe` 45

    it "Si Ana toma klusener de Huevo disminuye se resistencia a 50 (-5)" $ do
      (resistencia.(klusener "Huevo")) ana `shouldBe` 115
    it "Si Ana toma klusener de Frutilla disminuye se resistencia a 50 (-8)" $ do
      (resistencia.(klusener "Chocolate")) ana `shouldBe` 111

    it "Si Cristian toma un tintico, queda con 2 de resistencia por no tener" $ do
      (resistencia.tintico) cristian `shouldBe` 2
    it "Ana toma un Tintico, pasa a 130 de resistencia (tiene 2 amigos)" $ do
      (resistencia.tintico) ana `shouldBe` 130

    it "Rodri toma una Soda de fuerza 2, queda con nombre 'errpRodri'" $ do
      (nombre.(soda 2)) rodri `shouldBe` "errpRodri"
    it "Ana toma una Soda de fuerza 10, queda con nombre 'errrrrrrrrrpAna'" $ do
      (nombre.(soda 10)) ana `shouldBe` "errrrrrrrrrpAna"
    it "Ana toma una Soda de fuerza 0, queda con nombre 'epAna'" $ do
      (nombre.(soda 0)) ana `shouldBe` "epAna"

  describe "[Verificar punto 6] " $ do
    it "Si Ana se rescata por 0 horas debería tener la misma resistencia" $ do
      (resistencia.(rescatarse 0)) ana `shouldBe` 120
    it "Si Rodri se rescata por 5 horas debería tener 255 de resistencia" $ do
      (resistencia.(rescatarse 5)) rodri `shouldBe` 255
    it "Si Cristian se rescata por 1 horas debería tener 155 de resistencia" $ do
      (resistencia.(rescatarse 1)) rodri `shouldBe` 155
