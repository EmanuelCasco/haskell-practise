--Record Syntax
data Sonido = UnSonido {
    altos :: Float,
    medios :: Float,
    bajos :: Float
} deriving Show

data Musico = UnMusico {
  nombre:: String,
  instrumento :: (String, Bool)
} deriving Show

nombreInstrumento = fst . instrumento
instrumentoEsElectrico = snd . instrumento

sonido = UnSonido 10 10 10

pelado = UnMusico "Luca" ("guitarra",False)
cerati = UnMusico "Gus" ("guitarra",True)
charly = UnMusico "Carlitos" ("teclado",True)

genios = [pelado, charly, cerati]

---

modificarSonido :: (Float, Float, Float) -> Sonido -> Sonido
modificarSonido (a,m,b) (UnSonido alto medio bajo) = UnSonido (a * alto) (m * medio) (b * bajo)

---

marshall :: Sonido -> Sonido
marshall  = modificarSonido (1, 1.1, 1.1) 

laney :: Sonido -> Sonido
laney  =  modificarSonido (0.5, 0.5, 1.2)

peavey :: Sonido -> Sonido
peavey  = modificarSonido (0, 0, 1)

---

aplicarAmpli :: (Sonido -> Sonido) -> [Sonido] -> [Sonido]
aplicarAmpli = map 

---

filtrarMusicosSegun :: (Musico -> Bool) -> [Musico] -> [String]
filtrarMusicosSegun condicion = map nombre . filter condicion

esTecladista :: Musico -> Bool
esTecladista = (=="teclado") . nombreInstrumento

nombresTecladistas :: [Musico] -> [String]
nombresTecladistas = filtrarMusicosSegun esTecladista

nombresNecesitanAmpli :: [Musico] -> [String]
nombresNecesitanAmpli = filtrarMusicosSegun instrumentoEsElectrico

nombresNoNecesitanAmpli :: [Musico] -> [String]
nombresNoNecesitanAmpli = filtrarMusicosSegun (not . instrumentoEsElectrico)
