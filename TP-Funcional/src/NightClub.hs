module NightClub where

data Cliente = UnCliente {
  nombre' :: String,
  resistencia' :: Int,
  amigos' :: [Cliente]
} deriving (Show, Eq)

rodri = UnCliente "Rodri" 55 []
marcos = UnCliente "Marcos" 40 [rodri]
cristian = UnCliente "Cristian" 2 []
ana = UnCliente "Ana" 120 [marcos, rodri]
ema = UnCliente "Emanuel" 45 [cristian, rodri]

nombre :: Cliente -> String
nombre (UnCliente nombre _ _) = nombre

resistencia :: Cliente -> Int
resistencia (UnCliente _ resistencia _) = resistencia

amigos :: Cliente -> [Cliente]
amigos (UnCliente _ _ amigos) = amigos

---

comoEsta :: Cliente -> String
comoEsta cliente
  |  resistencia cliente > 50 = "fresco"
  |  (length.amigos) cliente > 1 = "piola"
  |  otherwise = "duro"

---

agregarAmigo :: Cliente -> Cliente -> Cliente
agregarAmigo amigo (UnCliente nombre resistencia amigos) =
  UnCliente nombre resistencia (amigos ++ [amigo])

esAmigo :: Cliente -> Cliente -> Bool
esAmigo cliente1 cliente2 = elem cliente1 (amigos cliente2)

reconocerAmigo :: Cliente -> Cliente -> Cliente
reconocerAmigo amigo cliente
  |  nombre amigo == nombre cliente = cliente
  |  amigo `esAmigo` cliente = cliente
  |  otherwise = agregarAmigo amigo cliente

---

modificarResistencia function (UnCliente nombre resistencia amigos) =
  UnCliente nombre (function resistencia) amigos

grogXD :: Cliente -> Cliente
grogXD = modificarResistencia (*0)

jarraLoca :: Cliente -> Cliente
jarraLoca cliente = modificarResistencia (flip (-) 10) cliente

klusener :: String -> Cliente -> Cliente
klusener gusto cliente = modificarResistencia (flip (-) (length gusto)) cliente

tintico :: Cliente -> Cliente
tintico cliente = modificarResistencia ((+) diferencia) cliente
  where diferencia = ((5*).length.amigos) cliente

soda :: Int -> Cliente -> Cliente
soda n (UnCliente nombre resistencia amigos) =
  UnCliente ("e" ++ replicate n 'r' ++ "p" ++ nombre) resistencia amigos

---

rescatarse :: Int -> Cliente -> Cliente
rescatarse horas cliente
  | (<= 0) horas = cliente
  | (>3) horas =  modificarResistencia ((+) 200) cliente
  | otherwise = modificarResistencia ((+) 100) cliente
