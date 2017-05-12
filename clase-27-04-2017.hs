{- Buscar la maxima extension de una lista en una lista de listas -}

maximumLength :: [[a]] -> Int
maximumLength = maximum.(map length)

{-

-}

data Persona = UnaPersona String Int String

cambiarPais :: String -> Persona -> Persona
cambiarPais residenciaNueva (UnaPersona nombre edad residencia) =
  UnaPersona nombre edad residenciaNueva

mudanza :: String -> [Persona] -> [Persona]
mudanza destino = map (cambiarPais destino)
