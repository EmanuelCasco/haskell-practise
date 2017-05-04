module NightClub2 where

data Cliente = UnCliente {
  nombre :: String,
  resistencia :: Int,
  amigos :: [Cliente]
} deriving Eq

rodri = UnCliente "Rodri" 55 []
marcos = UnCliente "Marcos" 40 [rodri, ana]
cristian = UnCliente "Cristian" 2 []
ana = UnCliente "Ana" 120 [marcos, rodri]

instance Show Cliente where
  show cliente = show (nombre cliente) ++ " " ++ show (resistencia cliente)

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

modificarResistencia :: (Int -> Int) -> Cliente -> Cliente
modificarResistencia function (UnCliente nombre resistencia amigos) =
  UnCliente nombre (function resistencia) amigos

modificarAmigos function (UnCliente nombre resistencia amigos) =
  UnCliente nombre resistencia (map function amigos)

grogXD :: Cliente -> Cliente
grogXD = modificarResistencia (*0)

jarraLoca :: Cliente -> Cliente
jarraLoca = modificarAmigos nuevaResistencia . nuevaResistencia
  where nuevaResistencia = modificarResistencia (abs . (-) 10)

klusener :: String -> Cliente -> Cliente
klusener gusto = modificarResistencia (abs . (-) (length gusto))

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
