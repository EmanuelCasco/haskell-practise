module NightClub where

data Cliente = UnCliente {
  nombre :: String,
  resistencia :: Int,
  amigos :: [Cliente]
} deriving Eq

instance Show Cliente where
  show cliente = show (nombre cliente) ++ " " ++ show (resistencia cliente)

rodri = UnCliente "Rodri" 55 []
marcos = UnCliente "Marcos" 40 [rodri]
cristian = UnCliente "Cristian" 2 []
ana = UnCliente "Ana" 120 [marcos, rodri]

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

grogXD :: Cliente -> Cliente
grogXD (UnCliente nombre resistencia amigos) =
  UnCliente nombre 0 amigos

jarraLoca :: Cliente -> Cliente
jarraLoca (UnCliente nombre resistencia amigos) =
  UnCliente nombre (resistencia - 10) (map function amigos)
  where function = (\(UnCliente nombre resistencia amigos) -> UnCliente nombre (resistencia-10) amigos)

klusener :: String -> Cliente -> Cliente
klusener gusto (UnCliente nombre resistencia amigos) =
  UnCliente nombre (resistencia - (length gusto)) amigos

tintico :: Cliente -> Cliente
tintico (UnCliente nombre resistencia amigos) =
  UnCliente nombre (resistencia + 5 * (length amigos)) amigos

soda :: Int -> Cliente -> Cliente
soda n (UnCliente nombre resistencia amigos) =
  UnCliente ("e" ++ replicate n 'r' ++ "p" ++ nombre) resistencia amigos

---

rescatarse :: Int -> Cliente -> Cliente
rescatarse horas (UnCliente nombre resistencia amigos)
  | (<= 0) horas = UnCliente nombre resistencia amigos
  | (>3) horas =  UnCliente nombre (resistencia + 200) amigos
  | otherwise = UnCliente nombre (resistencia + 100) amigos
