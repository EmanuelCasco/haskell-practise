data Persona = UnaPersona Int String deriving (Show, Eq)

instance Ord Persona where
  compare (UnaPersona _ nombre1) (UnaPersona _ nombre2) =
    length nombre1 `compare` length nombre2

juancho = UnaPersona 45 "Juan"
fran = UnaPersona 44 "Francisco"
ana = UnaPersona 23 "Ana"

between :: Ord a => a -> a -> a -> Bool
between xi xf x = xi <= x && x <= xf
