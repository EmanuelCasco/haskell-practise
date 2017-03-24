minimo :: Integer -> Integer -> Integer -> Integer
minimo x y z = min (min x y) z

maximo :: Integer -> Integer -> Integer -> Integer
maximo x y z = max (max x y) z

dispersion :: Integer -> Integer -> Integer -> Integer
dispersion dia1 dia2 dia3 = (maximo dia1 dia2 dia3) - (minimo dia1 dia2 dia3)

-- diasParejos: son días parejos si la dispersión es chica (menos de 30 cm)
diasParejos :: Integer -> Integer -> Integer -> Bool
diasParejos dia1 dia2 dia3 = (dispersion dia1 dia2 dia3) < 30

-- diasLocos: son días locos si la dispersión es grande (más de un metro)
diasLocos :: Integer -> Integer -> Integer -> Bool
diasLocos dia1 dia2 dia3 =  (dispersion dia1 dia2 dia3) > 100

-- diasNormales, son días normales si no son ni parejos ni locos.
diasNormales :: Integer -> Integer -> Integer -> Bool
diasNormales d1 d2 d3 = not ((diasParejos d1 d2 d3) || (diasLocos d1 d2 d3))
