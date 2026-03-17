
module Funciones
  ( potencia
  , factorial
  , gradosARadianes
  , radianesAGrados
  , seno
  , coseno
  , miMap
  , miTakeWhile
  , miIterate
  )
where

potencia :: Float -> Int -> Float
potencia _ 0 = 1
potencia x y = x * potencia x (y - 1)

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

gradosARadianes :: Float -> Float
gradosARadianes g = g * pi / 180

radianesAGrados :: Float -> Float
radianesAGrados r = r * 180 / pi

seno :: Float -> Int -> Float -> Float
seno x n suma
  | abs termino < 0.00001 = suma
  | otherwise = seno x (n + 1) (suma + termino)
  where
    termino = potencia (-1) n * potencia x (2 * n + 1) / fromIntegral (factorial (2 * n + 1))

coseno :: Float -> Float
coseno x = seno (x + pi / 2) 0 0
--recibe una función transformadora y una lista, y devuelve una lista con la función aplicada a cada elemento de la lista infinita
miMap :: (a -> b) -> [a] -> [b]
--caso base
miMap _ [] = [] --si la lista es vacía, devuelve una lista vacía
--caso recursivo
miMap f (x : xs) = f x : miMap f xs --aplica la función f a x y luego llama recursivamente en xs

--recibe una función, un valor inicial y devulve una lista infinita 
miIterate :: (a -> a) -> a -> [a]
miIterate f x = x : miIterate f (f x) --aplica la función f a x y luego llama recursivamente con el resultado de f x

--recibe una funcón confucion, una lista y devuelve una lista con los elementos de la lista hasta que se cumpla la condición
miTakeWhile :: (a -> Bool) -> [a] -> [a]
--caso base
miTakeWhile _ [] = []

--caso recursivo
miTakeWhile p (x:xs) -- se aplica el condicional p al primer elemento de la lista y resultante
  | p x = x : miTakeWhile p xs --si p en x es True, se agrega x a la lista resultante y se llama recursivamente con el resto de la lista
  | otherwise = [] --si p en x es False, se detiene la recursión y se devuelve la lista resultante hasta ese punto
  

