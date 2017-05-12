data Hombre = UnHombre {
    nombre :: String,
    peso :: Float
} deriving Show

homero = UnHombre "Homero Simpson" 100.0
barnie = UnHombre "Barnie Gomez" 110.0
comicMan = UnHombre "El señor de las historietas" 210.0

modificarPeso op value hombre = hombre { peso = peso hombre `op` value }

data Comida = UnaComida {
    calorias :: Float
} deriving Show

rosquilla1 = UnaComida 300
rosquilla2 = UnaComida 200
rosquilla3 = UnaComida 100

---

comer :: Comida -> Hombre -> Hombre
comer comida = modificarPeso (+) (calorias comida / 10)

panzada :: [Comida] -> Hombre -> Hombre
panzada comidas hombre = foldl (flip comer) hombre comidas

{-
DOC: 

foldr (+) 0 [1..5] -->
1 + (foldr (+) 0 [2..5]) -->
1 + (2 + (foldr (+) 0 [3..5])) -->
1 + (2 + (3 + (foldr (+) 0 [4..5]))) -->
1 + (2 + (3 + (4 + (foldr (+) 0 [5])))) -->
1 + (2 + (3 + (4 + 5)))

foldl (+) 0 [1..5] -->
in foldl (+) (0 + 1) [2..5] -->
in foldl (+) (0 + 1 + 2) [3..5] -->

-}