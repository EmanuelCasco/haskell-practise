esMultiploDe :: Integer -> Integer -> Bool
esMultiploDe = \x y -> (mod x y) == 0

esMultiploDe :: Integer -> Bool
esBisiesto anio = (esMultiploDe anio 400) || ((esMultiploDe anio 4) && (not (esMultiploDe anio 100)))

celsiusToFahr :: Integer -> Integer
celsiusToFahr = \celsius -> celsius * (9/5) + 32

fahrToCelsius :: Integer -> Integer
fahrToCelsius = \fahr -> (fahr - 32) / 1.8

fahrToCelsius :: Integer -> Bool
haceFrioF fahr= ((<8).fahrToCelsius) fahr

mcd :: Integer -> Integer -> Integer
mcd = gcd

mcm :: Integer -> Integer -> Float
mcm = \x y -> (x * y) `div` (mcd x y)
