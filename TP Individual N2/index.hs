data Arbol = Arbol String Int Int Float deriving Show
baobab = Arbol "Baobab" 5 10 1.7
ceibo = Arbol "Ceibo" 7 10 2.0
palmera = Arbol "Palmera" 10 5 40

crecimientoArboreo = 45

---

between :: Ord a => a -> a -> a -> Bool
between x minimo maximo  = minimo < x && x < maximo

---

esFrondoso :: Arbol -> Bool
esFrondoso (Arbol _ alto ancho vitalidad) =
  between alto 6 15 && ancho > alto && vitalidad > 1

esperanzaDeVida :: Arbol -> Float
esperanzaDeVida (Arbol _ _ _ vitalidad) =
  (vitalidad * crecimientoArboreo) / 2

lluvia :: Float -> Arbol -> Arbol
lluvia mm (Arbol nombre alto ancho vitalidad) =
  Arbol nombre (alto + 1) ancho (vitalidad + mm * vitalidad / 100)

granizo :: Arbol -> Arbol
granizo (Arbol nombre alto ancho vitalidad) =
  Arbol nombre (max 1 (alto - 2)) ancho vitalidad

tormenta :: Arbol -> Arbol
tormenta = granizo.(lluvia 100)

diaBueno :: Arbol -> Bool
diaBueno = (>5).esperanzaDeVida.(lluvia 150)

diaCon :: (Arbol -> Arbol) -> Arbol -> Bool
diaCon factorClimatico = (>5).esperanzaDeVida.factorClimatico
