data Aventurero = UnAventurero {
  nombre :: String,
  recorrido :: [(Int, Int)]
} deriving Show

type Vector = (Int, Int)

juan = UnAventurero "Juancho" [(0,0), (5,0), (7,0), (7,4), (11,2), (11,(-5))]
manu = UnAventurero "Emanuel" [(0,0), (10,0)]
john = UnAventurero "Lennon" [(0,0), (1,0), (2,0), (3,0), (4,0), (5,0)]

---

zonaMaldita :: (Vector,Vector)
zonaMaldita = ((10,0),(16,6))

between :: Ord a => a -> a -> a -> Bool
between xi xf x = xi <= x && x <= xf

perteneceAZona :: (Vector,Vector) -> Vector -> Bool
perteneceAZona ((xi,yi),(xf,yf)) (x, y) =  between (xi,yi) (xf,yf) (x, y)

---

estaMaldito :: Aventurero -> Bool
estaMaldito =  any (perteneceAZona zonaMaldita) . recorrido

nombresMalditos :: [Aventurero] -> [String]
nombresMalditos = map nombre . filter estaMaldito
