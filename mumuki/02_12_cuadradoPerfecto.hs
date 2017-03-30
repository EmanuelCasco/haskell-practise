-- Escribí la función esCuadradoPerfecto,
-- que responde si un numero es un cuadrado perfecto,
-- lo cual se cumple cuando su raiz cuadrada es un número entero

esCuadradoPerfecto1 :: Float -> Bool
esCuadradoPerfecto1 num =
  let raiz = sqrt num
  in  raiz == fromInteger (round raiz)

esInt :: Float -> Bool
esInt raiz = raiz == fromInteger (round raiz)
esCuadradoPerfecto2 :: Float -> Bool
esCuadradoPerfecto2 = (esInt.sqrt)
