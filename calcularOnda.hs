module CalcularOnda where

import Funciones (aplicarATodos, ejecutarParaCadaUno_, iterar, seno, tomarMientras)


onda :: String -> Float -> Float -> Float -> Float -> Float -> IO ()


onda "tiempo" k w x tMax dt = ejecutarParaCadaUno_ print resultados
  where
    tiempos = tomarMientras (<= tMax) (iterar (+ dt) 0)
    calcularAmplitud t = (seno (k * x - w * t) 0 0, t)
    resultados = aplicarATodos calcularAmplitud tiempos


onda "espacio" k w t lMax dx = ejecutarParaCadaUno_ print resultados
  where
    posiciones = tomarMientras (<= lMax) (iterar (+ dx) 0)
    calcularAmplitud x = (seno (k * x - w * t) 0 0, x)
    resultados = aplicarATodos calcularAmplitud posiciones

onda _ _ _ _ _ _ = error "El modo debe ser 'tiempo' o 'espacio'"
