esMultiploDe :: Integer -> Integer -> Bool
esMultiploDe = \x y -> (mod x y) == 0

esBisiesto :: Integer -> Bool
esBisiesto anio = (esMultiploDe anio 400) || ((esMultiploDe anio 4) && (not (esMultiploDe anio 100)))

celsiusToFahr :: Fractional a => a -> a
celsiusToFahr = \celsius -> celsius * (9/5) + 32

fahrToCelsius :: Fractional a => a -> a
fahrToCelsius = \fahr -> (fahr - 32) / 1.8

haceFrioF :: Float -> Bool
haceFrioF fahr= ((<8).fahrToCelsius) fahr

mcd :: Integer -> Integer -> Integer
mcd = gcd

mcm :: Integer -> Integer -> Integer
mcm = \x y -> ((x * y) `div` (mcd x y))

siguiente :: Integer -> Integer
siguiente = \x -> x + 1

mitad :: Fractional a => a -> a
mitad = \x -> x / 2

inversa :: Fractional a => a -> a
inversa = \x -> 1 / x

triple :: Num a => a -> a
triple = (*3)

esNumeroPositivo :: Float -> Bool
esNumeroPositivo = (>=0)

--largoEsPar, que toma un string y nos dice si su longitud es par.
largoEsPar :: String -> Bool
largoEsPar = (even.length)

-- nombreCompleto toma un nombre, segundo nombre y apellido
-- Devuelve el nombre completo (separado por espacios).
nombreCompleto :: String -> String -> String -> String
nombreCompleto name lastname surname =
  name ++ " " ++ lastname ++ " " ++ surname
