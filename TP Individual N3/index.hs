{-
Se tiene un juego en el que hay un mapa, en el que los aventureros deambulan,
recordando los puntos del mapa donde estuvieron. El aventurero está modelado de la siguiente forma:
  data Aventurero = UnAventurero String [(Int, Int)]

Un aventurero tiene un nombre y la lista de puntos del mapa donde estuvo
(cada tupla es un punto, que representa un par coordenada/ordenada).

Por ejemplo, juan = UnAventurero "Juancho" [(0,0), (5,0), (7,0), (7,4), (11,2), (11,(-5))]

En el mapa hay una zona con una terrible maldición. Esta zona es un rectángulo con los vértices (10,0) y (16,6)

Hacer la función nombresMalditos, que dada una lista de aventureros,
nos da los nombres de los aventureros que estuvieron en la zona de la terrible maldición.
-}
data Aventurero = UnAventurero String [(Int, Int)] deriving Show

juan = UnAventurero "Juancho" [(0,0), (5,0), (7,0), (7,4), (11,2), (11,(-5))]
manu = UnAventurero "Emanuel" [(0,0), (10,0)]
john = UnAventurero "Lennon!" [(0,0), (1,0), (2,0), (3,0), (4,0), (5,0)]

---

intersect :: Eq a => [a] -> [a] -> [a]
intersect xs ys =  [x | x <- xs, any (==x) ys]


createMap :: ((Int, Int),(Int, Int)) -> [(Int, Int)]
createMap ((x0,y0), (x1,y1)) =
  [ (x,y) | x <- [-50..50], y <- [-50..50], (x0 <= x && x <= x1) && (y0 <= y && y <= y1) ]

---

zonaMaldita :: [(Int, Int)]
zonaMaldita = createMap ((10,0), (16,6))

recorrido :: Aventurero -> [(Int, Int)]
recorrido (UnAventurero _ recorrido) = recorrido

nombre :: Aventurero -> String
nombre (UnAventurero nombre _) = nombre

maldito :: Aventurero -> Bool
maldito = (not.null).(intersect zonaMaldita).recorrido

nombresMalditos :: [Aventurero] -> [String]
nombresMalditos = (map nombre).(filter maldito)
