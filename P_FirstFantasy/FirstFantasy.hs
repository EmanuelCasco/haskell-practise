import Text.Show.Functions

data Personaje = UnPersonaje {
  nombre :: String,
  vida :: Float,
  fuerza :: Float,
  resistencia :: Float,
  objetos :: [Objeto]
} deriving Show

aeris = UnPersonaje {
  nombre = "Aeris",
  vida = 10000,
  resistencia = 2300,
  fuerza = 1100,
  objetos = [baculoDuplicador]
}
sephiroth = UnPersonaje {
  nombre = "Sephiroth",
  vida = 2500,
  resistencia = 2000,
  fuerza = 1000,
  objetos = [espadaOxidada, katanaFilosa, dagaLambdica 50]
}

type Objeto = (Float -> Float)

espadaOxidada = (1.2*)
katanaFilosa  = (10+).(0.9*)
dagaLambdica cm = ((1+cm)/100*)
anilloParadigmatico = sqrt
baculoDuplicador x = x * 2
espadaMaldita = espadaOxidada.dagaLambdica 89

----
-- Punto Nº 1
----

defensa, ataque :: Personaje -> Float
defensa pers = resistencia pers + vida pers
ataque pers = componerObjetos pers $ fuerza pers

componerObjetos pers = foldl1 (.) (objetos pers)
----
-- Punto Nº 2
----

atacar :: Personaje -> Personaje -> Personaje
atacar atacante defensor = defensor { vida = max 0 (vida defensor - danio) }
  where danio = max 0 (ataque atacante - defensa defensor)

----
-- Punto Nº 3
----
robarObjetos :: Personaje -> Personaje -> Personaje
robarObjetos atacante victima = atacante { objetos = objetos victima ++ objetos atacante }

convienePelear :: Personaje -> Personaje -> Bool
convienePelear atacante defensor = ataque atacante < ataque (robarObjetos atacante defensor)

----
-- Punto Nº 4
----

pelear :: Personaje -> Personaje -> Personaje
pelear atacante defensor 
  |  vida defensorAtacado == 0 = robarObjetos atacante defensor
  |  otherwise = pelear defensorAtacado atacante
  where defensorAtacado = atacante `atacar` defensor

----
-- Punto Nº 5
----

artesano fun pers = pers { objetos = fun }

aprendiz pers = artesano [componerObjetos pers] pers
maestro anios pers = artesano [((*) (1+0.01*anios)), componerObjetos pers] pers
estafador = artesano [id]
duplicador pers = artesano [ (2*) . objeto | objeto <- objetos pers ]

----

artesanosConAtaqueMayorA valor pers = filter (\artesano -> valor < ataque (artesano pers)) 

artesanosConvenientes pers = artesanosConAtaqueMayorA (ataque pers) pers
