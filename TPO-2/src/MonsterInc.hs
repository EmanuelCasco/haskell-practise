module MonsterInc where

type Grito = (String, Int, Bool)
onomatopeya :: Grito -> String
onomatopeya (o,_,_) = o
intensidadGrito :: Grito -> Int
intensidadGrito (_,i,_) = i
mojoLaCama :: Grito -> Bool
mojoLaCama (_,_,m) = m
nivelDeTerror :: Grito -> Int
nivelDeTerror = length . onomatopeya

type Risa = (Int, Int)
duracion :: Risa -> Int
duracion (d,_) = d
intensidadRisa :: Risa -> Int
intensidadRisa (_,i) = i

type Nene = (String, Int, Float)
nombre :: Nene -> String
nombre (n,_,_) = n
edad :: Nene -> Int
edad (_,e,_) = e
altura :: Nene -> Float
altura (_,_,a) = a

type Monstruo = (Nene -> Grito)
type Comediante = (Nene -> Risa)

----

energiaDeGrito :: Grito -> Int
energiaDeGrito grito 
    | mojoLaCama grito = nivelDeTerror grito * (intensidadGrito grito) ^ 2
    | otherwise = 3 * nivelDeTerror grito + intensidadGrito grito

----

numeroVocales string = length [ x | x <- string, x `elem` ['a','e','i','o','u'] ]
between x inf sup = inf < x && x < sup 

----

sullivan, randall, chuck, osito :: Monstruo
sullivan nene = (replicate ((length . nombre) nene) 'A' ++ "GH", 20 `div` edad nene, edad nene < 3)
randall nene = ("¡Mamadera!", (numeroVocales . nombre) nene, between (altura nene) 0.8 1.2)
chuck  nene = (['a'..'z'], 1000, True)
osito  nene = ("uf", edad nene, False)

----

pam :: [(a -> b)] -> a -> [b]
pam [] x = []
pam funciones x = (head funciones $ x) : pam (tail funciones) x

----

gritos :: Nene -> [Monstruo] -> [Grito]
gritos = flip pam

---- 

produccionEnergeticaGritos :: [Monstruo] -> [Nene] -> Int
produccionEnergeticaGritos monstruos nenes = foldr ((+) . energiaDeGrito) 0  (gritosNenes monstruos nenes)

gritosNenes :: [Monstruo] -> [Nene] -> [Grito]
gritosNenes monstruos [] = []
gritosNenes monstruos (nene:nenes) = gritos nene monstruos ++ gritosNenes monstruos nenes

---

energiaDeRisa :: Risa -> Integer
energiaDeRisa risa = toInteger (duracion risa) ^ toInteger (intensidadRisa risa)

risas :: Nene -> [Comediante] -> [Risa]
risas = flip pam

capusotto, stiller :: Comediante
capusotto nene = (2 * edad nene, 2 * edad nene)
stiller nene = (edad nene, edad nene)
politico nene = (0, 0)

---

produccionEnergeticaRisas :: [Comediante] -> [Nene] -> Integer
produccionEnergeticaRisas comediantes nenes = foldr ((+) . energiaDeRisa) 0 (risasNenes comediantes nenes)

risasNenes :: [Comediante] -> [Nene] -> [Risa]
risasNenes comediante [] = []
risasNenes comediante (nene:nenes) = risas nene comediante ++ risasNenes comediante nenes

--- 

produccionEnergetica :: [Comediante] -> [Nene] -> Integer
produccionEnergetica energiaDe producto empleados nenes  = foldr ((+) . energiaDe) 0 (producto empleados nenes)