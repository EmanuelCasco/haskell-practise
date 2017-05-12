cantidadDePochoclosParaMinutosDeCine :: Int -> Int
cantidadDePochoclosParaMinutosDeCine m
    | m < 40 = 2
    | m > 200 = 10
    | otherwise = m `div` 20

---
estadoDeAnimo :: String -> String
estadoDeAnimo "Viernes" = "¡Estoy enamorado!"
estadoDeAnimo _ = "Meh :S"

---

esVocalCerrada :: Char -> Bool
esVocalCerrada 'o' = True
esVocalCerrada 'u' = True
esVocalCerrada  _  = False

---

puntosParaSetenta :: Float -> Float
puntosParaSetenta 1 = 5.5
puntosParaSetenta 10 = 0.5
puntosParaSetenta 11 = 0.5
puntosParaSetenta 12 = 0.5
puntosParaSetenta x = x

---

distanciaAlOrigen :: (Float,Float) -> Float
distanciaAlOrigen (x,y) = sqrt( x^2 + y^2 )

---

poderSoldado :: (String, Int, Int) -> Int
poderSoldado (_, fuerza, destreza) = fuerza * destreza

soldadoLeGanaA :: (String, Int, Int) -> (String, Int, Int) -> Bool
soldadoLeGanaA ganador perdedor = poderSoldado ganador > poderSoldado perdedor

---
fst3 :: (a,b,c) -> a
fst3 (x,_,_) = x
snd3 :: (a,b,c) -> b
snd3 (_,y,_) = y
trd3 :: (a,b,c) -> c
trd3 (_,_,z) = z

---

aplicarTupla :: (a -> b, a -> c) -> a -> (b,c)
aplicarTupla (f1,f2) n = (f1 n, f2 n)

---

cuentaBizarra :: (Int,Int) -> Int
cuentaBizarra (x,y)
    | x > y = x + y
    | y > x + 10 = y -x
    | otherwise = x * y

---

esNotaBochazo :: Int -> Bool
esNotaBochazo num = num < 4

aprobo :: (Int, Int) -> Bool
aprobo (x,y) =
    (not.esNotaBochazo) x && (not.esNotaBochazo) y

promociono :: (Int, Int) -> Bool
promociono (n1,n2) =
    (n1 + n2) >= 14 && n1 >= 6 && n2 >= 6

---

esMayorDeEdad :: (String,Int) -> Bool
esMayorDeEdad = ((>=21).snd)

---

-- calcular/1
-- si el primer elemento es par lo duplica; si no lo deja como está
-- si el segundo elemento es impar le suma 1; si no deja como está
calcular :: (Int,Int) -> (Int,Int)
calcular (x,y)
    | even x && (not.even) y = (2*x,y+1)
    | even x = (2*x,y)
    | (not.even) y = (x, y+1)
    | otherwise = (x,y)
