
module Funciones
  ( potencia
  , factorial
  , gradosARadianes
  , radianesAGrados
  , seno
  , coseno
  , mimap
  , miTakeWhile
  , miiterate
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
--recibe una función y una lista, y devuelve una lista con la función aplicada a cada elemento de la lista.
mimap :: (a -> b) -> [a] -> [b]
--caso base
mimap _ [] = []
--caso recursivo
mimap f (x : xs) = f x : mimap f xs

miiterate :: (a -> a) -> a -> [a]
miiterate f x = x : miiterate f (f x)

miTakeWhile :: (a -> Bool) -> [a] -> [a]
-- caso base
miTakeWhile _ [] = []

miTakeWhile p (x:xs)
  

