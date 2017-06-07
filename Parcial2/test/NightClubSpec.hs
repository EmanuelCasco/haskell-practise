module NightClubSpec where

import Test.Hspec
import Control.Exception (evaluate)

import NightClub
import Text.Show.Functions
import Data.List

---

testAll :: IO ()
testAll = hspec $ do

----------------------------------------------------------
---          TESTS PRIMERA PARTE TP FUNCIONAL          ---
----------------------------------------------------------

  describe "[TP Nº 1] Verificar puntos 1 y 2] " $ do
    it "Nombre de Rodri debe ser 'Rodri'" $ do
      (nombre rodri) `shouldBe` "Rodri"
    it "Resistencia de Ana debe ser 120" $ do
      (resistencia ana) `shouldBe` 120
    it "Amigos de Ana deben ser [rodri]" $ do
      (amigos marcos) `shouldMatchList` [rodri]

  describe "[TP Nº 1] Verificar punto 3] " $ do
    it "Cristian debe estar 'duro'" $ do
      (comoEsta cristian) `shouldBe` "duro"
    it "Rodri debe estar 'fresco'" $ do
      (comoEsta rodri) `shouldBe` "fresco"
    it "Marcos debe estar 'duro'" $ do
      (comoEsta marcos) `shouldBe` "duro"
    it "Si Marcos se hace amigo de Ana y Rodri, está 'piola'" $ do
      (comoEsta . reconocerAmigo rodri . reconocerAmigo ana) marcos `shouldBe` "piola"

  describe "[TP Nº 1] Verificar punto 4] " $ do
    it "Cristian reconoce a Marcos como amigo" $ do
      (amigos . reconocerAmigo marcos) cristian `shouldMatchList` [marcos]
    it "Cristian no puede reconocerse a si mismo como amigo" $ do
      reconocerAmigo cristian cristian `shouldBe` cristian
    it "Cristian no puede reconocerse a Marcos dos veces como amigo" $ do
      (reconocerAmigo marcos . reconocerAmigo marcos) cristian `shouldBe` cristian

  describe "[TP Nº 1] Verificar punto 5] " $ do
    it "Ana toma grogXD. Queda con resistencia 0" $ do
      (resistencia . tomarGrogXD) ana `shouldBe` 0
    it "Si Ana toma la jarraLoca. Marcos queda con resistencia 30 (-10)" $ do
      (resistencia . head . amigos . tomarJarraLoca) ana `shouldBe` 30
    it "Marcos toma la jarraLoca. Queda con resistencia 30" $ do
      (resistencia . tomarJarraLoca) marcos `shouldBe` 30
    it "Rodri toma la jarraLoca. Queda con resistencia 110" $ do
      (resistencia . tomarJarraLoca) rodri `shouldBe` 45

    it "Si Ana toma klusener de Huevo disminuye se resistencia a 50 (-5)" $ do
      (resistencia . tomarKlusener "Huevo") ana `shouldBe` 115
    it "Si Ana toma klusener de Chocolate disminuye se resistencia a 50 (-8)" $ do
      (resistencia . tomarKlusener "Chocolate") ana `shouldBe` 111

    it "Si Cristian toma un tintico, queda con 2 de resistencia por no tener" $ do
      (resistencia . tomarTintico) cristian `shouldBe` 2
    it "Ana toma un Tintico, pasa a 130 de resistencia (tiene 2 amigos)" $ do
      (resistencia . tomarTintico) ana `shouldBe` 130

    it "Rodri toma una Soda de fuerza 2, queda con nombre 'errpRodri'" $ do
      (nombre . tomarSoda 2) rodri `shouldBe` "errpRodri"
    it "Ana toma una Soda de fuerza 10, queda con nombre 'errrrrrrrrrpAna'" $ do
      (nombre . tomarSoda 10) ana `shouldBe` "errrrrrrrrrpAna"
    it "Ana toma una Soda de fuerza 0, queda con nombre 'epAna'" $ do
      (nombre . tomarSoda 0) ana `shouldBe` "epAna"

  describe "[TP Nº 1] Verificar punto 6] " $ do
    it "Si Rodri se rescata por 5 horas debería tener 255 de resistencia" $ do
      (resistencia . rescatarse 5) rodri `shouldBe` 255
    it "Si Marcos se rescata por 3 horas debería tener 140 de resistencia" $ do
      (resistencia . rescatarse 3) marcos `shouldBe` 140
    it "Si Cristian se rescata por 1 horas debería tener 155 de resistencia" $ do
      (resistencia . rescatarse 1) rodri `shouldBe` 155
    it "Si Ana se rescata por 0 horas debería aparecer un error" $ do
      evaluate ( (resistencia . rescatarse 0) ana ) `shouldThrow` anyException
    it "Si Ana se rescata por menos de 0 horas debería aparecer un error" $ do
      evaluate ( (resistencia . rescatarse (-1)) ana ) `shouldThrow` anyException

----------------------------------------------------------
---          TESTS SEGUNDA PARTE TP FUNCIONAL          ---
----------------------------------------------------------

  describe "[TP Nº 2] Verificar punto 1.a:" $ do
     it "Rodri tomó un tintico" $ do
        tomarTragos (tragos rodri) rodri `shouldBe` tomarTintico rodri
  
  describe "[TP Nº 2] Verificar punto 1.b:" $ do
     it "Rodri tomó un tintico" $ do
        (length . tragos . tomarGrogXD) ana `shouldBe` 1 
     it "Marcos toma una soda de nivel 3 y queda con 2 bebidas" $ do
        (length . tragos . tomarSoda 3) marcos `shouldBe` 2
     it "Marcos toma una soda de nivel 3 y queda con 40 de resistencia" $ do
        (resistencia . tomarSoda 3) marcos `shouldBe` 40

  let anaElDiaDespues = tomarTragos [tomarJarraLoca, tomarKlusener "Chocolate", rescatarse 2, tomarKlusener "Huevo"] ana
  describe "[TP Nº 2] Verificar punto 1.c:" $ do
     it "Ana toma una jarra loca, un klusener de chocolate, se rescata 2 horas y luego toma un klusener de huevo: Su resistencia queda en 196" $ do
        resistencia anaElDiaDespues `shouldBe` 196
     it "Ana toma una jarra loca, un klusener de chocolate, se rescata 2 horas y luego toma un klusener de huevo: Quedan como amigos Marcos (30 de resistencia) y Rodri (45 de resistencia)" $ do
        (map resistencia . amigos) anaElDiaDespues `shouldBe` [30, 45]
     it "Ana toma una jarra loca, un klusener de chocolate, se rescata 2 horas y luego toma un klusener de huevo: En su lista de tragos ahora hay 3 elementos" $ do
        (length . tragos) anaElDiaDespues `shouldBe` 3
     it "Rodri toma una soda de nivel 1 y una soda de nivel 2 y queda con nombre errperpRodri" $ do
        (nombre . tomarSoda 2 . tomarSoda 1) rodri `shouldBe` "errperpRodri"
     it "Marcos toma un klusener de huevo, un tintico y una jarraLoca y queda con 30 de resistencia" $ do
        (resistencia . tomarJarraLoca . tomarTintico . tomarKlusener "Huevo" ) marcos `shouldBe` 30
     it "Marcos toma un klusener de huevo, un tintico y una jarraLoca y queda con 4 bebidas en el historial" $ do
        (length . tragos . tomarJarraLoca . tomarTintico . tomarKlusener "Huevo" ) marcos `shouldBe` 4

  describe "[TP Nº 2] Verificar punto 1.d:" $ do
     it "Marcos pide “dame otro” y lo deja con 34 de resistencia" $ do
        (resistencia . dameOtro) marcos `shouldBe` 34
     it "Marcos pide “dame otro” y tiene 2 bebidas en el historial" $ do
        (length . tragos . dameOtro) marcos `shouldBe` 2
     it "Ana pide `dameOtro`, debe dar error" $ do
        evaluate (dameOtro ana) `shouldThrow` anyException
     it "Rodri toma una soda de nivel 1, y “dame otro” da como resultado que tiene 3 bebidas" $ do
         (length . tragos . dameOtro . tomarSoda 1) rodri `shouldBe` 3
     it "Rodri toma una soda de nivel 1, y “dame otro” da como resultado que su nombre queda “erperpRodri”" $ do
         (nombre . dameOtro . tomarSoda 1) rodri `shouldBe` "erperpRodri"

  describe "[TP Nº 2] Verificar punto 2: `cuantasPuedeTomar`" $ do
     it "Rodri puede tomar dos tragos, entre un grogXD, un tintico y un klusener de frutilla" $ do
         cuantasPuedeTomar [tomarGrogXD, tomarTintico, tomarKlusener "Frutilla"] rodri `shouldBe` 2
     it "Cristian no puede tomar un trago, entre un tintico y un klusener de huevo" $ do
         cuantasPuedeTomar [tomarGrogXD, tomarTintico, tomarKlusener "Huevo"] cristian `shouldBe` 1
     it "Rodri puede tomar una sola bebida entre un grog XD, un tintico, un klusener de fru..utilla" $ do
         cuantasPuedeTomar [tomarGrogXD, tomarTintico, tomarKlusener "fruuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuutilla"] rodri `shouldBe` 1

  describe "[TP Nº 2] Verificar punto 3:" $ do
     it "Rodri realiza una salida de amigos, debe quedar con 1 amigo" $ do
         (length . amigos . realizarItinerario salidaDeAmigos) rodri `shouldBe` 1
     it "Rodri hace una salida de amigos y se debe llamar “erpRodri”" $ do
         (nombre . realizarItinerario salidaDeAmigos) rodri `shouldBe` "erpRodri"
     it "Rodri realiza una salida de amigos, debe quedar con 45 de Resistencia" $ do
         (resistencia . realizarItinerario salidaDeAmigos) rodri `shouldBe` 45
     it "Rodri realiza una salida de amigos, ahora es amigo de Roberto Carlos" $ do
         (amigos . realizarItinerario salidaDeAmigos) rodri `shouldSatisfy` elem robertoCarlos
     it "Rodri realiza una salida de amigos, su amigo Roberto Carlos debe quedar con 155 de resistencia" $ do
         (resistencia . head . amigos . realizarItinerario salidaDeAmigos) rodri `shouldBe` 155
     it "Rodri realiza una salida de amigos, debe quedar con 4 bebidas en su historial" $ do
         (length . tragos . realizarItinerario salidaDeAmigos) rodri `shouldBe` 4

  describe "[TP Nº 2] Verificar punto 4.a:" $ do
     it "La intensidad del itinerario basico es 0.8" $ do
         intensidadItinerario itinerarioBasico `shouldBe` 0.8
     it "La intensidad de la mezcla explosiva es 1.6" $ do
         intensidadItinerario mezclaExplosiva `shouldBe` 1.6
     it "La intensidad de la salidaDeAmigos es 4.0" $ do
         intensidadItinerario salidaDeAmigos `shouldBe` 4.0

  describe "[TP Nº 2] Verificar punto 4.b:" $ do
     it "El itinerario más intenso, de los conocidos, es la salida de amigos" $ do
         (descripcion . itinerarioMasIntenso) [salidaDeAmigos, mezclaExplosiva, itinerarioBasico] `shouldBe` "Salida de amigos"
     it "Rodri hace el itinerario más intenso entre una salida de amigos, la mezcla explosiva y el itinerario básico y queda con el nombre 'erpRodri'" $ do
         (nombre . realizarItinerario ((itinerarioMasIntenso) [salidaDeAmigos, mezclaExplosiva, itinerarioBasico])) rodri `shouldBe` "erpRodri"
     it "Rodri hace el itinerario más intenso entre una salida de amigos, la mezcla explosiva y el itinerario básico y queda con resistencia 45" $ do
         (resistencia . realizarItinerario ((itinerarioMasIntenso) [salidaDeAmigos, mezclaExplosiva, itinerarioBasico])) rodri `shouldBe` 45
     it "Rodri hace el itinerario más intenso entre una salida de amigos, la mezcla explosiva y el itinerario básico  y queda con un amigo (Roberto Carlos)" $ do
         (map nombre . amigos . realizarItinerario ((itinerarioMasIntenso) [salidaDeAmigos, mezclaExplosiva, itinerarioBasico])) rodri `shouldBe` ["Roberto Carlos"]

  describe "[TP Nº 2] Verificar punto 5:" $ do
     it "La resistencia de Chuck es mayor a la de Ana" $ do
         resistencia chuckNorris `shouldSatisfy` (> resistencia ana)
     it "Chuck realiza un itinerario basico, al finalizar su resistencia es 1076" $ do
         (resistencia . realizarItinerario itinerarioBasico) chuckNorris `shouldBe` 1076

  describe "[TP Nº 2] Verificar punto 6:" $ do
      it "Roberto Carlos se hace amigo de Ana, toma una jarra popular de espirituosidad 0, sigue teniendo una sola amiga (Ana)" $ do
         (length . amigos . tomarJarraPopular 0 . reconocerAmigo ana) robertoCarlos `shouldBe` 1
      it "Roberto Carlos se hace amigo de Ana, toma una jarra popular de espirituosidad 1, ganó dos amigos (3)" $ do
         (length . amigos . tomarJarraPopular 1 . reconocerAmigo ana) robertoCarlos `shouldBe` 3
      it "Roberto Carlos se hace amigo de Ana, toma una jarra popular de espirituosidad 1, sus nuevos amigos son rodri y marcos" $ do
         (map nombre . amigos . tomarJarraPopular 1 . reconocerAmigo ana) robertoCarlos `shouldBe` ["Marcos", "Rodri", "Ana"]
      it "Cristian se hace amigo de Ana. Roberto Carlos se hace amigo de Cristian, toma una jarra popular de espirituosidad 4, queda con 4 amigos (Cristian, Ana, Marcos y Rodri)" $ do
         (map nombre . amigos . tomarJarraPopular 4 . reconocerAmigo (reconocerAmigo ana cristian)) robertoCarlos `shouldBe` ["Marcos","Rodri","Ana","Cristian"]