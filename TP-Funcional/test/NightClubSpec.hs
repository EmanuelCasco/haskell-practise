module NightClubSpec where

import NightClub
import Test.Hspec

---

intersect :: Eq a => [a] -> [a] -> [a]
intersect xs ys =  [x | x <- xs, any (==x) ys]

---

main :: IO ()
main = hspec $ do
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
    it "Si Ana toma grogXD su resistencia baja a 0" $ do
      resistencia (grogXD ana) `shouldBe` 0
    it "Si Ana toma la jarraLoca su resistencia baja a 100 (-20)" $ do
      resistencia (jarraLoca ana) `shouldBe` 100
    it "Si Rodri toma klusener de Huevo disminuye se resistencia a 50 (-5)" $ do
      resistencia (klusener "Huevo" rodri) `shouldBe` 50
    it "Si Rodri toma klusener de Frutilla disminuye se resistencia a 50 (-8)" $ do
      resistencia (klusener "Frutilla" rodri) `shouldBe` 47
    it "Si Cristian toma una soda de fuerza 2 se le agrega 'errp' delante del nombre" $ do
      nombre (soda 2 cristian) `shouldBe` "errpCristian"
    it "Si Marcos toma una soda de fuerza 5 se le agrega 'errrrrp' delante del nombre" $ do
      nombre (soda 5 marcos) `shouldBe` "errrrrpMarcos"
    it "Si Anabela se rescata por 0 horas debería tener la misma resistencia" $ do
      resistencia (rescatarse 0 ana) `shouldBe` 120
    it "Si Anabela se rescata por 2 horas debería tener 100 de resistencia" $ do
      resistencia (rescatarse 2 ana) `shouldBe` 100
    it "Si Cristian se rescata por 4 horas debería tener 200 de resistencia" $ do
      resistencia (rescatarse 4 cristian) `shouldBe` 200
