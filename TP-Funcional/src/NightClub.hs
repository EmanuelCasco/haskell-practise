module NightClub where

data Cliente = UnCliente {
  nombre' :: String,
  resistencia' :: Int,
  amigos' :: [Cliente]
} deriving (Show, Eq)

rodri = UnCliente "Rodri" 55 []
marcos = UnCliente "Marcos" 40 [rodri]
cristian = UnCliente "Cristian" 2 []
ana = UnCliente "Anabela" 120 [marcos, rodri]
ema = UnCliente "Emanuel" 45 [cristian, rodri]

nombre :: Cliente -> String
nombre (UnCliente nombre _ _) = nombre

resistencia :: Cliente -> Int
resistencia (UnCliente _ resistencia _) = resistencia

amigos :: Cliente -> [Cliente]
amigos (UnCliente _ _ amigos) = amigos

comoEsta :: Cliente -> String
comoEsta cliente
  |  resistencia cliente > 50 = "fresco"
  |  (length.amigos) cliente > 1 = "piola"
  |  otherwise = "duro"

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


bajarResistencia x (UnCliente nombre resistencia amigos) =
  UnCliente nombre (resistencia - x) amigos

grogXD :: Cliente -> Cliente
grogXD cliente = bajarResistencia (resistencia cliente) cliente

jarraLoca :: Cliente -> Cliente
jarraLoca cliente = cliente

klusener :: String -> Cliente -> Cliente
klusener gusto cliente = bajarResistencia (length gusto) cliente

tintico :: Cliente -> Cliente
tintico (UnCliente nombre resistencia amigos) =
  UnCliente nombre (resistencia - 10) (map (bajarResistencia 10) amigos)

soda :: Int -> Cliente -> Cliente
soda n (UnCliente nombre resistencia amigos) =
  UnCliente ("erp"++nombre) resistencia amigos

rescatarse :: Int -> Cliente -> Cliente
rescatarse horas (UnCliente nombre resistencia amigos)
  | (<= 0) horas = UnCliente nombre resistencia amigos
  | (>3) horas =  UnCliente nombre 200 amigos
  | otherwise = UnCliente nombre 100 amigos
