
-- Módulo que contiene funciones matemáticas y de orden superior
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

-- Calcula x elevado a la potencia n de forma recursiva
-- potencia 2 3 = 8 (2^3)
potencia :: Float -> Int -> Float
potencia _ 0 = 1 -- cualquier número a la potencia 0 es 1
potencia x y = x * potencia x (y - 1) -- recursión: x^y = x * x^(y-1)

-- Calcula el factorial de un número de forma recursiva
-- factorial 5 = 120 (5! = 5*4*3*2*1)
factorial :: Int -> Int
factorial 0 = 1 -- caso base: 0! = 1
factorial n = n * factorial (n - 1) -- recursión: n! = n * (n-1)!

-- Convierte grados a radianes
-- gradosARadianes 180 = π (pi)
gradosARadianes :: Float -> Float
gradosARadianes g = g * pi / 180 -- fórmula: radianes = grados * π / 180

-- Convierte radianes a grados
-- radianesAGrados π = 180
radianesAGrados :: Float -> Float
radianesAGrados r = r * 180 / pi -- fórmula: grados = radianes * 180 / π

-- Calcula el seno de un ángulo usando la serie de Taylor
-- Parámetros: x (ángulo en radianes), n (término actual), suma (acumulador)
seno :: Float -> Int -> Float -> Float
seno x n suma
  | abs termino < 0.00001 = suma -- condición de parada: cuando el término es muy pequeño
  | otherwise = seno x (n + 1) (suma + termino) -- recursión: suma el término y continúa
  where
    -- Término de la serie de Taylor para seno: (-1)^n * x^(2n+1) / (2n+1)!
    termino = potencia (-1) n * potencia x (2 * n + 1) / fromIntegral (factorial (2 * n + 1))

-- Calcula el coseno usando la identidad trigonométrica: cos(x) = sin(x + π/2)
coseno :: Float -> Float
coseno x = seno (x + pi / 2) 0 0

-- Función de orden superior similar a 'map' en Haskell
-- Recibe una función transformadora y una lista, aplicando la función a cada elemento
-- aplicarATodos (+1) [1,2,3] = [2,3,4]
aplicarATodos :: (a -> b) -> [a] -> [b]
aplicarATodos _ [] = [] -- caso base: lista vacía devuelve lista vacía
aplicarATodos f (x : xs) = f x : aplicarATodos f xs -- aplica función f a cabeza y recursivamente a cola


-- Aplica una acción monadica a cada elemento de una lista, en orden
-- Se utiliza para efectos secundarios (como imprimir)
-- El guion bajo _ ignora el resultado de la acción
ejecutarParaCadaUno_ :: (a -> IO b) -> [a] -> IO ()
ejecutarParaCadaUno_ _ [] = return () -- caso base: lista vacía no hace nada
ejecutarParaCadaUno_ f (x : xs) = do
  _ <- f x -- ejecuta la acción f sobre el primer elemento
  ejecutarParaCadaUno_ f xs -- luego ejecuta recursivamente en el resto


-- recibe una función y un valor inicial y devuelve una lista infinita.
iterar :: (a -> a) -> a -> [a]
iterar f x = x : iterar f (f x) -- aplica la función f a x y luego llama recursivamente con el resultado


-- Toma elementos de una lista MIENTRAS el predicado sea verdadero
-- tomarMientras (< 5) [1,2,3,6,4] = [1,2,3]  (se detiene cuando encuentra 6)
tomarMientras :: (a -> Bool) -> [a] -> [a]
tomarMientras _ [] = [] -- caso base: lista vacía devuelve lista vacía
tomarMientras p (x : xs)
  | p x = x : tomarMientras p xs -- si el predicado es verdadero, incluye elemento y continúa
  | otherwise = [] -- si es falso, detiene completamente
  

-- Toma elementos de una lista HASTA (incluyendo) el primero que cumpla el predicado
-- tomarHasta (> 3) [1,2,3,4,5] = [1,2,3,4]  (se detiene después de incluir 4)
tomarHasta :: (a -> Bool) -> [a] -> [a]
tomarHasta _ [] = [] -- caso base: lista vacía devuelve lista vacía
tomarHasta p (x : xs) = x : if p x then [] else tomarHasta p xs -- incluye elemento y detiene si predicado es verdadero
