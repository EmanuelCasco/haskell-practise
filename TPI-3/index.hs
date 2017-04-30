data Aventurero = UnAventurero {
  nombre :: String,
  recorrido :: [(Int, Int)]
} deriving Show

juan = UnAventurero "Juancho" [(0,0), (5,0), (7,0), (7,4), (11,2), (11,(-5))]
manu = UnAventurero "Emanuel" [(0,0), (10,0)]
john = UnAventurero "Lennon" [(0,0), (1,0), (2,0), (3,0), (4,0), (5,0)]

---

intersect :: Eq a => [a] -> [a] -> [a]
intersect xs ys =  [x | x <- xs, x `elem` ys]

createZone :: ((Int, Int),(Int, Int)) -> [(Int, Int)]
createZone ((xi,yi), (xf,yf)) = [ (x,y) | x <- [-50..50], y <- [-50..50],
                                          elem x [xi..xf], elem y [yi..yf] ]

---

zonaMaldita :: [(Int, Int)]
zonaMaldita = createZone ((10,0), (16,6))

recorridoDe :: Aventurero -> [(Int, Int)]
recorridoDe (UnAventurero _ recorrido) = recorrido

nombreDe :: Aventurero -> String
nombreDe (UnAventurero nombre _) = nombre

estaMaldito :: Aventurero -> Bool
estaMaldito = (not.null) . intersect zonaMaldita . recorrido

---

nombresMalditos :: [Aventurero] -> [String]
nombresMalditos = map nombre . filter estaMaldito
