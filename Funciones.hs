
module Funciones
  ( potencia
  , factorial
  , gradosARadianes
  , radianesAGrados
  , seno
  , coseno
  )
where

potencia :: Float -> Int -> Float
potencia _ 0 = 1
potencia x y = x * potencia x (y - 1)

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- funcion convertir grados a radianes
gradosARadianes :: Float -> Float
gradosARadianes g = g * pi / 180

-- funcion convertir radianes a grados
radianesAGrados :: Float -> Float
radianesAGrados r = r * 180 / pi

-- funcion seno usando la serie de Taylor
seno :: Float -> Int -> Float -> Float
seno x n suma
  | abs termino < 0.00001 = suma
  | otherwise = seno x (n + 1) (suma + termino)
  where
    termino = potencia (-1) n * potencia x (2 * n + 1) / fromIntegral (factorial (2 * n + 1))


-- funcion cos(x) usando sen(x) y la identidad trigonometrica cos(x) = sen(x + pi/2)
coseno :: Float -> Float
coseno x = seno (x + pi / 2) 0 0



