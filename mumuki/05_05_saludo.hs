saludar :: String -> String
saludar name = "Hola " ++ name

saludar2 :: String -> String -> String
saludar2 name pre = "Hola " ++ pre ++ " " ++ name

saludar3 :: (String -> String) -> String -> String
saludar3 f name = "Hola " ++ (f name)

-- plana: toma un nombre y lo devuelve tal cual vino.
plana :: String -> String
plana = id
-- sr: toma un nombre y le agrega adelante "Sr."
sr :: String -> String
sr = ("Sr. "++)
-- ing: toma un nombre y le agrega adelante "Ing."
ing :: String -> String
ing = ("Ing. "++)
-- sra: toma un nombre y le agrega adelante "Sra."
sra :: String -> String
sra = ("Sra. "++)
-- reyQuick: toma un nombre y le agrega al final "Rey de los Minisupers"
reyQuick :: String -> String
reyQuick = (++" Rey de los Minisupers")
