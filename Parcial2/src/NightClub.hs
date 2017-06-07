module NightClub where

import Text.Show.Functions
import Data.List

type Trago = (Cliente -> Cliente)

data Cliente = UnCliente {
  nombre :: String,
  resistencia :: Int,
  amigos :: [Cliente],
  tragos :: [Trago]
} 

instance Eq Cliente where
  (==) cliente1 cliente2 = nombre cliente1 == nombre cliente2

instance Show Cliente where
  show cliente = "{ nombre: " ++ show (nombre cliente)
              ++ ", resistencia: " ++ show (resistencia cliente)
              ++ ", amigos: " ++ show (map nombre (amigos cliente))
              ++ ", tragos: [Tragos x" ++ show (length . tragos $ cliente) ++ "] }"

modificarNombre :: String -> Cliente -> Cliente
modificarNombre nuevoNombre cliente = cliente { nombre = nuevoNombre }

modificarResistencia :: (Int -> Int -> Int) -> Int -> Cliente -> Cliente
modificarResistencia operation valor cliente = cliente { resistencia = (resistencia cliente) `operation` valor }

modificarAmigos :: (Cliente -> Cliente) -> Cliente -> Cliente
modificarAmigos function cliente = cliente { amigos = (map function . amigos) cliente }

modificarTragos :: ([Trago] -> [Trago]) -> Cliente -> Cliente
modificarTragos function cliente = cliente { tragos = function $ tragos cliente }

---

rodri = UnCliente { 
  nombre = "Rodri", 
  resistencia = 55, 
  amigos = [],
  tragos = [tomarTintico]
}
marcos = UnCliente { 
  nombre = "Marcos", 
  resistencia = 40, 
  amigos = [rodri],
  tragos = [(tomarKlusener "Guinda")]
}
cristian = UnCliente { 
  nombre = "Cristian", 
  resistencia = 2, 
  amigos = [],
  tragos = [tomarGrogXD, tomarJarraLoca]
}
ana = UnCliente { 
  nombre = "Ana", 
  resistencia = 120, 
  amigos = [marcos,rodri],
  tragos = []
}
robertoCarlos = UnCliente {
  nombre = "Roberto Carlos",
  resistencia = 165,
  amigos = [],
  tragos = []
}
chuckNorris = UnCliente {
  nombre = "Chuck Norris",
  resistencia = 1000,
  amigos = [ana],
  tragos = [ tomarSoda x | x <- [1..] ]
}

{-
Justificar: ¿Puede chuckNorris pedir otro trago con la función dameOtro?
La funcion `dameOtro` no sera valida con chuck, ya que el compilador intentara reducir una lista infinita para encontrar el ultimo elementos
Justificar: ¿puedo hacer que chuckNorris realice el itinerario básico y conocer su resistencia resultante?
Si, gracias al `lazy evaluation` de Haskell, al no necesitar la lista de tragos (infinita) de chuck, no habra nignu tipo de problema para aplicarle el itinerario.
Justificar: ¿puedo preguntar si chuckNorris tiene más resistencia que ana?
Si, exactamente por lo mismo del punto anterior
-}

---

comoEsta :: Cliente -> String
comoEsta cliente
  |  resistencia cliente > 50 = "fresco"
  |  (length . amigos) cliente > 1 = "piola"
  |  otherwise = "duro"

---

esAmigo :: Cliente -> Cliente -> Bool
esAmigo amigo = elem amigo . amigos

reconocerAmigo :: Cliente -> Cliente -> Cliente
reconocerAmigo amigo cliente
  |  amigo == cliente || amigo `esAmigo` cliente = cliente
  |  otherwise = cliente { amigos =  amigo : amigos cliente }

agregarTrago :: Trago -> Cliente -> Cliente
agregarTrago trago = modificarTragos ((:) trago)

---

tomarGrogXD :: Trago
tomarGrogXD = agregarTrago tomarGrogXD . modificarResistencia (*) 0

tomarJarraLoca :: Trago
tomarJarraLoca = agregarTrago tomarJarraLoca . modificarAmigos efectoJarra . efectoJarra 
  where efectoJarra = modificarResistencia (-) 10

tomarKlusener :: String -> Trago
tomarKlusener gusto = agregarTrago (tomarKlusener gusto) . modificarResistencia (-) (length gusto) 

tomarTintico :: Trago
tomarTintico cliente = agregarTrago tomarTintico $ modificarResistencia (+) dif cliente
  where dif = 5 * (length . amigos) cliente

tomarSoda :: Int -> Trago
tomarSoda fuerza cliente = agregarTrago (tomarSoda fuerza) $ modificarNombre nuevoNombre cliente
  where nuevoNombre = "e" ++ replicate fuerza 'r' ++ "p" ++ nombre cliente

tomarJarraPopular :: Int -> Trago
tomarJarraPopular espirituosidad cliente
  |  espirituosidad == 0 = agregarTrago (tomarJarraPopular espirituosidad) cliente
  |  otherwise = tomarJarraPopular (espirituosidad - 1) (hacerseAmigo cliente)
  
---

rescatarse :: Int -> Cliente -> Cliente
rescatarse horas cliente
  | (>3) horas = modificarResistencia (+) 200 cliente
  | (>0) horas = modificarResistencia (+) 100 cliente
  | otherwise  = error "Not valid hour input"

----

tomarTragos :: [Trago] -> Cliente -> Cliente
tomarTragos [] = id
tomarTragos tragos  =  foldl1 (.) tragos

dameOtro :: Cliente -> Cliente
dameOtro cliente 
  | (not . null . tragos) cliente = ultimoTrago cliente
  | otherwise = error "Cliente no tomó nada"
  where ultimoTrago = (head . tragos) cliente

---

cualesPuedeTomar :: [Trago] -> Cliente -> [Trago]
cualesPuedeTomar listaTragos cliente = filter resistenciaMayorCero listaTragos
  where resistenciaMayorCero trago = (resistencia . trago) cliente > 0

cuantasPuedeTomar :: [Trago] -> Cliente -> Int
cuantasPuedeTomar listaTragos = length . cualesPuedeTomar listaTragos

---

data Itinerario = UnItinerario {
  descripcion :: String,
  duracion :: Float,
  acciones :: [Cliente -> Cliente]
}

mezclaExplosiva = UnItinerario { 
  descripcion = "Mezcla Explosiva", 
  duracion = 2.5, 
  acciones = [tomarKlusener "Frutilla", tomarKlusener "Huevo", tomarGrogXD, tomarGrogXD]
}
itinerarioBasico = UnItinerario { 
  descripcion = "Basico", 
  duracion = 5, 
  acciones = [tomarKlusener "Huevo", rescatarse 2, tomarKlusener "Chocolate", tomarJarraLoca]
}
salidaDeAmigos = UnItinerario { 
  descripcion = "Salida de amigos", 
  duracion = 1, 
  acciones = [tomarJarraLoca, reconocerAmigo robertoCarlos, tomarTintico, tomarSoda 1]
}

realizarItinerario :: Itinerario -> Cliente -> Cliente
realizarItinerario itinerario = foldl1 (.) (acciones itinerario)

----

intensidadItinerario :: Itinerario -> Float
intensidadItinerario itinerario = genericLength (acciones itinerario) / duracion itinerario

----

itinerarioMasIntenso :: [Itinerario] -> Itinerario
itinerarioMasIntenso = foldl1 itinerarioConMasIntensidad 

itinerarioConMasIntensidad i1 i2 
  | intensidadItinerario i1 > intensidadItinerario i2 = i1
  | otherwise = i2

----

hacerseAmigo :: Cliente -> Cliente
hacerseAmigo cliente = foldr reconocerAmigo cliente amigosDeAmigos
  where amigosDeAmigos = (concat . map amigos . amigos) cliente
