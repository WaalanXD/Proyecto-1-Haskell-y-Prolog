module CalcularOnda where

import Funciones (aplicarATodos, ejecutarParaCadaUno_, iterar, seno, tomarMientras)

-- onda modo k w fijo maximo delta retorna IO ()
onda :: String -> Float -> Float -> Float -> Float -> Float -> IO ()

-- 1. CASO TIEMPO: x es fijo, t varía hasta maximo (T)
onda "tiempo" k w x tMax dt = ejecutarParaCadaUno_ print resultados
  where
    tiempos = tomarMientras (<= tMax) (iterar (+ dt) 0)
    calcularAmplitud t = (seno (k * x - w * t) 0 0, t)
    resultados = aplicarATodos calcularAmplitud tiempos

-- 2. CASO ESPACIO: t es fijo, x varía hasta maximo (L)
onda "espacio" k w t lMax dx = ejecutarParaCadaUno_ print resultados
  where
    posiciones = tomarMientras (<= lMax) (iterar (+ dx) 0)
    calcularAmplitud x = (seno (k * x - w * t) 0 0, x)
    resultados = aplicarATodos calcularAmplitud posiciones

-- 3. CASO DE ERROR: por si escriben mal el modo
onda _ _ _ _ _ _ = error "El modo debe ser 'tiempo' o 'espacio'"
