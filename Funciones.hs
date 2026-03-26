
module Funciones
  ( potencia
  , factorial
  , gradosARadianes
  , radianesAGrados
  , seno
  , coseno
  , aplicarATodos
  , ejecutarParaCadaUno_
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


aplicarATodos :: (a -> b) -> [a] -> [b]
aplicarATodos _ [] = [] 
aplicarATodos f (x : xs) = f x : aplicarATodos f xs 



ejecutarParaCadaUno_ :: (a -> IO b) -> [a] -> IO ()
ejecutarParaCadaUno_ _ [] = return ()
ejecutarParaCadaUno_ f (x : xs) = do
  _ <- f x
  ejecutarParaCadaUno_ f xs



iterar :: (a -> a) -> a -> [a]
iterar f x = x : iterar f (f x)



tomarMientras :: (a -> Bool) -> [a] -> [a]
tomarMientras _ [] = []
tomarMientras p (x : xs)
  | p x = x : tomarMientras p xs
  | otherwise = []
  


tomarHasta :: (a -> Bool) -> [a] -> [a]
tomarHasta _ [] = []
tomarHasta p (x : xs) = x : if p x then [] else tomarHasta p xs
