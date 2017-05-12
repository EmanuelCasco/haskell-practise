incrementMCuadradoN :: Int -> Int -> Int
incrementMCuadradoN m n =
  ((increment n).cuadrado) m

---

increment :: Int -> Int -> Int
increment n = \x -> x+n
cuadrado :: Int -> Int
cuadrado  = \x -> x^2

---

esResultadoPar :: Int -> Int -> Bool
esResultadoPar n m = (even.(n^)) m

---

existsAny :: (t -> Bool) -> t -> t -> t -> Bool
existsAny f x y z = (f x) || (f y) || (f z)

---

mejor :: Ord a => (a -> a) -> (a -> a) -> a -> a
mejor f1 f2 n = max (f1 n) (f2 n)

---

aplicarPar :: (a -> b) -> a -> a -> (b,b)
aplicarPar f x y = (f x, f y)

---

parDeFns :: (a -> b) -> (a -> c) -> a -> (b, c)
parDeFns f1 f2 num = (f1 num, f2 num)

---

darVuelta :: (t2 -> t1 -> t) -> t1 -> t2 -> t
darVuelta f x y = f y x

---

-- Escribí y explicitá el tipo de la función componer
-- Hace lo mismo que la composición: compone dos funciones. No usar la función '.'
componer :: (b -> c) -> (a -> b) -> a -> c
componer f1 f2 m = f1 (f2 m)

---

-- Escribí una función tuplizar que tome una función de dos parametros,
-- y la convierta en otra que toma sólamente un par.
tuplizar :: (a -> b -> c) -> (a, b) -> c
tuplizar f (arg1, arg2) = (f) arg1 arg2

---

-- Definir una función cuantoPagaCadaUno,
-- que aplicándola con un precio de pizza y una cantidad de comensales,
-- devuelva cuánto debe pagar cada uno.
-- Tener en cuenta que cada comensal come 3 porciones
-- y sólo se pueden comprar pizzas enteras (cada una 8 porciones).
cuantoPagaCadaUno :: Integer -> Integer -> Float
cuantoPagaCadaUno total n = division (precio total n) n
precio :: Integer -> Integer -> Integer
precio total n = (techo (division (n * 3) 8) ) * total
techo :: Double -> Integer
techo = ceiling

division a b = (fromIntegral a) / (fromIntegral b)

---
