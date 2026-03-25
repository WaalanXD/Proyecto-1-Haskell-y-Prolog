
module Funciones
  ( potencia
  , factorial
  , gradosARadianes
  , radianesAGrados
  , seno
  , coseno
  , aplicarATodos
  , mapearM_
  , tomarMientras
  , iterar
  , tomarHasta
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

-- recibe una función transformadora y una lista, y devuelve una lista
-- con la función aplicada a cada elemento.
aplicarATodos :: (a -> b) -> [a] -> [b]
aplicarATodos _ [] = [] -- si la lista es vacía, devuelve una lista vacía
aplicarATodos f (x : xs) = f x : aplicarATodos f xs -- aplica la función f a x y luego llama recursivamente en xs


-- aplica una acción a cada elemento, en orden.
mapearM_ :: (a -> IO b) -> [a] -> IO ()
mapearM_ _ [] = return ()
mapearM_ f (x : xs) = do
  _ <- f x
  mapearM_ f xs


-- recibe una función y un valor inicial y devuelve una lista infinita.
iterar :: (a -> a) -> a -> [a]
iterar f x = x : iterar f (f x) -- aplica la función f a x y luego llama recursivamente con el resultado


-- recibe un predicado y una lista, y devuelve una lista con los elementos
-- mientras el predicado sea verdadero.
tomarMientras :: (a -> Bool) -> [a] -> [a]
tomarMientras _ [] = [] -- si la lista es vacía, devuelve una lista vacía
tomarMientras p (x : xs)
  | p x = x : tomarMientras p xs
  | otherwise = []
  

-- toma elementos de una lista hasta (e incluyendo) el primero que cumpla el predicado.
tomarHasta :: (a -> Bool) -> [a] -> [a]
tomarHasta _ [] = []
tomarHasta p (x : xs) = x : if p x then [] else tomarHasta p xs
