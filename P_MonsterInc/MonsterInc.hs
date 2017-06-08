import Text.Show.Functions

type Grito = (String, Int, Bool)
onomatopeya (o,_,_) = o 
intensidad (_,i,_) = i 
mojoLaCama (_,_,m) = m 

-------------------
--  Punto Nº 01  --
-------------------

nivelDeTerror :: Grito -> Int
nivelDeTerror = length . onomatopeya

energiaDeGrito :: Grito -> Int
energiaDeGrito grito
  |  mojoLaCama grito = nivelDeTerror grito * (intensidad grito) ^ 2
  |  otherwise = nivelDeTerror grito * 3 + intensidad grito

-------------------
--  Punto Nº 02  --
-------------------

type Nene = (String, Int, Float)
nombre (n,_,_) = n 
edad (_,e,_) = e 
altura (_,_,a) = a

type Monstruo = (Nene -> Grito)

sullivan, randall, chuck, osito :: Monstruo
sullivan nene = (replicate (edad nene) 'A' ++ "GH", div 20 (edad nene), edad nene < 3)
randall nene = ("¡Mamadera!", (length . filter esVocal) $ nombre nene, between (altura nene) 0.8 1.2)
chuck nene = (['a'..'z']++['A'..'Z'], 1000, True)
osito nene = ( "uf", edad nene, False)

----

between x minX maxX = minX < x && x < maxX 
esVocal c = elem c "aeiouAEIOU"

-------------------
--  Punto Nº 03  --
-------------------

pam :: [(a->b)] -> a -> [b]
pam functions var = map (\f -> f var) functions 

-------------------
--  Punto Nº 04  --
-------------------

gritos :: Nene -> [Monstruo] -> [Grito]
gritos = flip pam

-------------------
--  Punto Nº 05  --
-------------------

produccionEnergeticaGritos :: [Monstruo] -> [Nene] -> Int
produccionEnergeticaGritos monstruos nenes = 
  foldr ((+) . energiaDeGrito) 0 [ monstruo nene | monstruo <- monstruos, nene <- nenes ]

-------------------
--  Punto Nº 06  --
-------------------

type Risa = (Int, Int)
duracion = fst
intensidadRisa = snd

energiaDeRisa :: Risa -> Int
energiaDeRisa risa = duracion risa ^ intensidadRisa risa

----

type Comediante = (Nene -> Risa)
capusotto :: Comediante
capusotto nene = (2 * edad nene, 2 * edad nene)

----

produccionEnergeticaRisas :: [Comediante] -> [Nene] -> Int
produccionEnergeticaRisas comediantes nenes = 
  foldr ((+) . energiaDeRisa) 0 [ comediante nene | comediante <- comediantes, nene <- nenes ]

-------------------
--  Punto Nº 07  --
-------------------

produccionEnergetica trabajadores nenes generador = 
  foldr ((+) . generador) 0 [ trabajador nene | trabajador <- trabajadores, nene <- nenes ]
