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
createZone ((xi,yi), (xf,yf)) = [ (x,y) | x <- [xi..xf], y <- [yi..yf] ]

---

zonaMaldita :: [(Int, Int)]
zonaMaldita = createZone ((10,0), (16,6))

estaMaldito :: Aventurero -> Bool
estaMaldito = (not.null) . intersect zonaMaldita . recorrido

---

nombresMalditos :: [Aventurero] -> [String]
nombresMalditos = map nombre . filter estaMaldito
