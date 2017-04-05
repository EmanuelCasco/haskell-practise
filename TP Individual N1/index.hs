nivelDia1 = 22
nivelDia2 = 283
nivelDia3 = 294

minimo :: Integer -> Integer -> Integer -> Integer
minimo x y z = (min x y) `min` z

maximo :: Integer -> Integer -> Integer -> Integer
maximo x y z = (max x y) `max` z

-- Dispersion, diferencia entre el maximo y el minimo en 3 dias
dispersion :: Integer -> Integer -> Integer -> Integer
dispersion dia1 dia2 dia3 = maximo dia1 dia2 dia3 - minimo dia1 dia2 dia3

-- diasParejos: son días parejos si la dispersión es chica (menos de 30 cm)
diasParejos :: Integer -> Integer -> Integer -> Bool
diasParejos dia1 dia2 dia3 = dispersion dia1 dia2 dia3 < 30

-- diasLocos: son días locos si la dispersión es grande (más de un metro)
diasLocos :: Integer -> Integer -> Integer -> Bool
diasLocos dia1 dia2 dia3 =  dispersion dia1 dia2 dia3 > 100

-- diasNormales, son días normales si no son ni parejos ni locos.
diasNormales :: Integer -> Integer -> Integer -> Bool
diasNormales d1 d2 d3 = not (diasParejos d1 d2 d3 || diasLocos d1 d2 d3)
