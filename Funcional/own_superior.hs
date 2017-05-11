componer :: (b->c) -> (a->b) -> a-> c
componer f g valor = f (g valor)

map' = id
filter' = id
any' = id
flip' = id
foldl' = id
foldr' = id
pesos = id