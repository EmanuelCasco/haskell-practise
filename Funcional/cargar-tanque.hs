{-
Quiero cargar nafta al tanque de un auto, sabiendo cuánto tiene, cuál es la capacidad, y cuánto va a cargar. 
Se pide: saber el valor final del tanque, teniendo en cuenta que nunca puede pasarse de la capacidad máxima. 
Por ejemplo, si tengo un auto con un tanque que tiene capacidad máxima 50 y 20 litros cargados, al cargar 10 litros el tanque resultante tendrá 30 litros. Pero si al mismo intento cargarle 35 litros, el tanque resultante tendrá 50 litros, que es el máximo.
Resolverlo usando composición y aplicación parcial. Las funciones min o max pueden ser útiles.
-}

cargarNafta :: Int -> Int -> Int -> Int 
cargarNafta cargaActual capacidadMaxima = (min capacidadMaxima).(+cargaActual) 
