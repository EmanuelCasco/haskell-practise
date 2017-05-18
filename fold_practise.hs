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

factorial 0 = 1
factorial n = n * factorial (n - 1)

---map' & elem' = 
elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' n (x:xs) | n == x = True | otherwise = elem' n xs

map' :: (a->b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = (f $ x) : map' f xs

any' :: (a->Bool) -> [a] -> Bool
any' f [] = False
any' f (x:xs) | f x = True | otherwise = any' f xs

--primerosPares numero  
primerosPares n = take n . filter even

primerosPares2 n = take n . pares
pares (x:xs)
    |  even x = x : pares xs
    |  otherwise =  pares xs
